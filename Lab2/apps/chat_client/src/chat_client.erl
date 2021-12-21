-module(chat_client).

%% API
-export([login/0, get_authorized_clients/0, get_message_history/0, broadcast/1]).
%% Gen server API
-export([start_link/3]).
%% Gen server callbacks
-export([handle_call/3, handle_info/2, handle_cast/2, init/1, terminate/2]).

-behavior(gen_server).

-define(CLIENT, chat_client).
-define(SERVER, chat_server).

-record(state, {
    clientInfo = {} :: {string(), string(), atom()}, % Username, Password, NodeName
    authUsers = [] :: [{string(), atom()}], % Username, NodeName
    messageHistory = [] :: [{string(), string(), integer()}]  % Username, Message, Origin Timestamp
  }).

%% ===================================================================
%% Gen server API and internal methods
%% ===================================================================

start_link(ServerNodeName, Username, Password) ->
  State = #state{clientInfo={ServerNodeName, Username, Password}, authUsers=[], messageHistory=[]},
  {ok, Pid} = gen_server:start_link(?MODULE, State, []),
  register(?CLIENT, Pid),
  lager:info("Started the client genserver: ~p", [{ok, Pid}]),
  lager:info("On node ~p", [node()]),
  {ok, Pid}.

init(State = #state{clientInfo = _ClientInfo, authUsers = _AuthUsers, messageHistory = _MessageHistory}) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

%% ===================================================================
%% Gen server callbacks
%% ===================================================================
%% ===================================================================
%% Gen server calls
%% ===================================================================

handle_call(
  {auth_client},
  _From,
  State = #state{clientInfo = {ServerNodeName, Username, Password}, authUsers = _, messageHistory = _}
) ->
  lager:info("ServerNodeName: ~p; Username: ~p; Password: ~p", [ServerNodeName, Username, Password]), 
  {?SERVER, ServerNodeName} ! {auth_client, Username, Password, node()},
  {reply, ok, State};

handle_call({get_state}, _From, State) ->
  {reply, State#state.messageHistory, State}.
%% ===================================================================
%% Gen server casts
%% ===================================================================

handle_cast(
  {get_authorized_clients},
  State = #state{clientInfo = {ServerNodeName, Username, Password}, authUsers = _, messageHistory = _}
) ->
  {?SERVER, ServerNodeName} ! {get_authorized_clients, Username, Password},
  {noreply, State};
% Node broadcasts message to other nodes. This is the original node that sends the broadcast
% Called by the client API 'broadcast(Message)'
handle_cast(
  {broadcast, Message},
  _State = #state{clientInfo = ClientInfo = {_, Username, _}, authUsers = Users, messageHistory = MessageHistory}
) ->
  MessageTime = erlang:system_time(),
  lists:foldr(
    fun(Entry, Acc) ->
      {RecipientUserName, RecipientNode} = Entry,
      case Username of
        RecipientUserName -> Acc; % Do nothing
        _ -> {?CLIENT, RecipientNode} ! {receive_message, Username, Message, MessageTime},
             Acc % Do nothing with the accumulator.
      end
    end,
    0,
    Users
  ),
  NewState = #state{
    clientInfo = ClientInfo,
    authUsers = Users,
    messageHistory = [{Username, Message, MessageTime} | MessageHistory]
  },
  {noreply, NewState};
% Node re-sends message to other nodes when the message history is empty
% This broadcast is called by handle_info, so no need to update state
handle_cast(
  {broadcast, SenderUsername, Message, SenderTimestamp},
  _State = #state{clientInfo = {_,Username,_}, authUsers = Users, messageHistory = []}
) ->

  send_message_all_but_current(Username, Users, SenderUsername, Message, SenderTimestamp),
  {noreply, _State};
% Node sends received message to others when there is existing message history
% No need to update state, since it's updated in handle_info, which this method is called from
handle_cast(
  {broadcast, SenderUsername, Message, SenderTimestamp},
  _State = #state{clientInfo = {_,Username,_}, authUsers = Users, messageHistory = MessageHistory}
) ->
  
  case lists:member({SenderUsername, Message, SenderTimestamp}, MessageHistory) of
    true -> true;
    false -> 
      send_message_all_but_current(Username, Users, SenderUsername, Message, SenderTimestamp)
    end,
  {noreply, _State}.

%% ===================================================================
%% Gen server infos
%% ===================================================================

handle_info(
  {get_authorized_clients_from_server, Users},
  _State = #state{clientInfo = ClientInfo, authUsers = _, messageHistory = MessageHistory}
) ->
  NewState = #state{clientInfo = ClientInfo, authUsers = Users, messageHistory = MessageHistory},
  lager:info("Got all authorized clients, here they are:~p~n", [NewState]),
  {noreply, NewState};

handle_info(
  {receive_message, SenderUsername, Message, SenderTimestamp},
  State = #state{clientInfo = ClientInfo, authUsers = AuthUsers, messageHistory = []}
) ->
  StateWithoutSender = #state{
    clientInfo = ClientInfo,
    authUsers = sub_list_without_tuple_by_key(AuthUsers, SenderUsername),
    messageHistory = []
  },
  handle_cast({broadcast, SenderUsername, Message, SenderTimestamp}, StateWithoutSender),
  
  NewState = State#state{messageHistory = [{SenderUsername, Message, SenderTimestamp}]},
  {noreply, NewState};

handle_info(
  {receive_message, SenderUsername, Message, SenderTimestamp},
  State = #state{clientInfo = ClientInfo, authUsers = AuthUsers, messageHistory = MessageHistory}
) ->
  NewState = case lists:member({SenderUsername, Message, SenderTimestamp}, MessageHistory) of
    true -> State;
    false -> 
      StateWithoutSender = 
        #state{
          clientInfo = ClientInfo,
          authUsers = sub_list_without_tuple_by_key(AuthUsers, SenderUsername),
          messageHistory = MessageHistory
        },
      handle_cast({broadcast, SenderUsername, Message, SenderTimestamp}, StateWithoutSender),
      State#state{messageHistory = [{SenderUsername, Message, SenderTimestamp} | MessageHistory]}
    end,
  {noreply, NewState};
handle_info(_Info, State) ->
  lager:error("Unhandled message passing to client found!~nINFO:~p~nSTATE:~p", [_Info, State]),
  {noreply , State}.

%% ===================================================================
%% Client API
%% ===================================================================

login() ->
  lager:info("LOGING IN..."),
  ok = gen_server:call(?CLIENT, {auth_client}),
  lager:info("LOGGED IN!"),
  {noreply, ok}.

get_message_history() ->
  gen_server:call(?CLIENT, {get_state}).

get_authorized_clients() ->
  ok = gen_server:cast(?CLIENT, {get_authorized_clients}),
  {noreply, ok}.

broadcast(Message) ->
  ok = gen_server:cast(?CLIENT, {broadcast, Message}),
  {noreply, ok}.

%% ===================================================================
%% Internal Methods
%% ===================================================================
% iterate and send message to users except for current node based on authenticated user list
send_message_all_but_current(ClientUsername, AuthUserList, SenderUsername, Message, SenderTimestamp) ->
  lists:foldr(
    fun(Entry, Acc) ->
      {RecipientUserName, RecipientNode} = Entry,
      case ClientUsername of
        RecipientUserName -> Acc; % Do nothing
        _ ->
              {?CLIENT, RecipientNode} ! {receive_message, SenderUsername, Message, SenderTimestamp},
              Acc % Do nothing with the accumulator.
      end
  end,
  0,
  AuthUserList
  ).
% create sublist without a specific user
sub_list_without_tuple_by_key(List, ValueOfKey) ->
  [ T || {K, _} = T <-  List, K =/= ValueOfKey].

