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

handle_call({auth_client}, _From, State = #state{clientInfo={ServerNodeName,Username,Password}, authUsers = _, messageHistory = _}) ->
  lager:info("ServerNodeName: ~p; Username: ~p; Password: ~p", [ServerNodeName, Username, Password]),

  {?SERVER, ServerNodeName} ! {auth_client, Username, Password, node()},
  {reply, ok, State};

handle_call({get_state}, _From, State) ->
  {reply, State#state.messageHistory, State}.
%% ===================================================================
%% Gen server casts
%% ===================================================================


handle_cast({get_authorized_clients}, State = #state{clientInfo={ServerNodeName,Username,Password}, authUsers = _, messageHistory = _}) ->
  {?SERVER, ServerNodeName} ! {get_authorized_clients, Username, Password},
  {noreply, State};
% Kai pirmas node'as siuncia broadcast'a kitiems
handle_cast({broadcast, Message}, _State = #state{clientInfo = ClientInfo = {_,Username,_}, authUsers = Users, messageHistory = MessageHistory}) ->
  io:format("~nKai node'as siuncia broadcast'a kitiems:~n"),
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
  % Create new state, won't use lists:append since it's quadratic complexity
  NewState = #state{clientInfo = ClientInfo, authUsers = Users, messageHistory = [{Username, Message, MessageTime} | MessageHistory]},
  {noreply, NewState};
% Kai node'as persiuncia zinute kitiems, bet cia pirma nauja zinute
handle_cast({broadcast, SenderUsername, Message, SenderTimestamp}, _State = #state{clientInfo = {_,Username,_}, authUsers = Users, messageHistory = []}) ->
  io:format("~nKai node'as persiuncia zinute kitiems, bet cia pirma nauja zinute paciam node'ui:~n"),
  lists:foldr(
    fun(Entry, Acc) ->
      {RecipientUserName, RecipientNode} = Entry,
      case Username of
        RecipientUserName -> Acc; % Do nothing
        _ ->
          {?CLIENT, RecipientNode} ! {receive_message, SenderUsername, Message, SenderTimestamp},
          Acc % Do nothing with the accumulator.
      end
    end,
    0,
    Users
  ),
  % Create new state, won't use lists:append since it's quadratic complexity
  % NewState = #state{clientInfo = ClientInfo, authUsers = Users, messageHistory = [{Username, Message, SenderTimestamp}]},
  {noreply, _State};
% Kai node'as persiuncia zinute kitiems
handle_cast({broadcast, SenderUsername, Message, SenderTimestamp}, _State = #state{clientInfo = {_,Username,_}, authUsers = Users, messageHistory = MessageHistory}) ->
  io:format("~nKai node'as persiuncia zinute:~n"),
  lists:foldr(
    fun(Entry, Acc) ->
      {RecipientUserName, RecipientNode} = Entry,
      case Username of
        RecipientUserName -> Acc; % Do nothing
        _ ->
          case lists:member({SenderUsername, Message, SenderTimestamp}, MessageHistory) of
            true -> Acc;
            false -> 
              {?CLIENT, RecipientNode} ! {receive_message, SenderUsername, Message, SenderTimestamp},
              Acc % Do nothing with the accumulator.
          end
      end
    end,
    0,
    Users
  ),
  % Create new state, won't use lists:append since it's quadratic complexity
  % NewState = #state{clientInfo = ClientInfo, authUsers = Users, messageHistory = [{Username, Message, SenderTimestamp} | MessageHistory]},
  {noreply, _State}.

%% ===================================================================
%% Gen server infos
%% ===================================================================

handle_info({get_authorized_clients_from_server, Users}, _State = #state{clientInfo = ClientInfo, authUsers = _, messageHistory = MessageHistory}) ->
  NewState = #state{clientInfo = ClientInfo, authUsers = Users, messageHistory = MessageHistory},
  lager:info("Got all authorized clients, here they are:~p~n", [NewState]),
  {noreply, NewState};

handle_info({receive_message, SenderUsername, Message, SenderTimestamp}, State = #state{clientInfo = ClientInfo, authUsers = AuthUsers, messageHistory = []}) ->
  io:format("~nKai node'as gauna zinute, neturedamas zinuciu istorijos:~n"),
  StateWithoutSender = #state{clientInfo = ClientInfo, authUsers = [ T || {K, _} = T <-  AuthUsers, K =/= SenderUsername], messageHistory = []},
  handle_cast({broadcast, SenderUsername, Message, SenderTimestamp}, StateWithoutSender),
  
  NewState = State#state{messageHistory = [{SenderUsername, Message, SenderTimestamp}]},
  {noreply, NewState};

handle_info({receive_message, SenderUsername, Message, SenderTimestamp}, State = #state{clientInfo = ClientInfo, authUsers = AuthUsers, messageHistory = MessageHistory}) ->
  io:format("~nKai node'as gauna zinute, turedamas zinuciu istorija:~n"),
  {CurrentLatestUsername, CurrentLatestMessage, CurrentLatestTimestamp} = lists:nth(1, MessageHistory),
  io:format("~nStai yra paskutinis username'as ir gavejo username'as: ~p ir ~p, ~npaskutine zinute ir gavejo zinute: ~p ir ~p ~npaskutinis timestamp'as ir gavejo timestamp'as: ~p ir ~p~n", [CurrentLatestUsername, SenderUsername, CurrentLatestMessage, Message, CurrentLatestTimestamp, SenderTimestamp]),
  io:format("~nAr jau yra egzistuojanti zinutes istorija?~p~n", [lists:member({SenderUsername, Message, SenderTimestamp}, MessageHistory)]),
  io:format("~nVISA ZINUCIU ISTORIJA~p~n", [MessageHistory]),
  NewState = case lists:member({SenderUsername, Message, SenderTimestamp}, MessageHistory) of
    true -> State;
    false -> 
      StateWithoutSender = #state{clientInfo = ClientInfo, authUsers = [ T || {K, _} = T <-  AuthUsers, K =/= SenderUsername], messageHistory = MessageHistory},
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
% IMPLEMENT URB?
% Iterate through users and message pass to each user
% Kaip patikrinti ar first reception'as?
% Patikrinus ar pirma karta gauta zinute m, tada kiekvienam procesui,
% Iskyrus einamajam procesui ir procesui, kuris siunte zinute, issiusti visiems kitiems
% State visi node'ai, i map'o state'a pasirasyt kad buvo gauta ta zinute
