-module(chat_server).

-export([start_link/0]).
-export([handle_call/3, handle_cast/2, init/1, handle_info/2]).

-behavior(gen_server).

-define(SERVER, chat_server).
-define(CLIENT, chat_client).

-record(state, {
    users = [] :: [{string(), string(), atom()}]
}).

%% ===================================================================
%% Gen server API
%% ===================================================================

start_link() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  register(?SERVER, Pid),
  lager:info("Started the server genserver on:~p~n", [Pid]),
  lager:info("On node ~p", [node()]),
  {ok, Pid}.

init(_Args) ->
  {ok, #state{users = []}}.

%% ===================================================================
%% Gen server callbacks
%% ===================================================================

handle_call({auth_client, Username, Password, Node}, _From, State = #state{users = Users}) ->
  NewUsers = case lists:keyfind(Username, 1, Users) of
    {Username, Password, _Node} -> 
      lager:info("Reconnecting user ~p...", [Username]),
      lager:info("Updating their node information..."),
      lists:keyreplace({Username, Password}, 1, Users, {{Username, Password}, Node});
    false ->
      lager:info("New user ~p is connecting to the server...", [Username]),
      lager:info("Adding them to record of known users..."),
      [{Username, Password, Node} | Users]
  end,
  % NewUsers = case lists:member({Username}, Users) of
  %   true  -> Users;
  %   false -> [Username | Users]
  % end,  io:format("FINDING IN INITIAL LIST:~p~n", [Users]),
  % NewUsers = lists:usort([Username |users]),
  % lager:info("USER LIST: ~p~n", [NewUsers]),
  {reply, ok, State#state{users = NewUsers}}.

handle_cast({process_public_message, Username, Password, Message}, State = #state{users = Users}) ->
  case lists:keyfind(Username, 1, Users) of
    {Username, Password, _Node}  -> process_public_message(Username, Users, Message);
    false -> 
      lager:warning("Unauthorized public message access attempt for: ~p", [Username])
  end,
  {noreply, State};
handle_cast({process_private_message, Username, Password, RecipientUsername, Message}, State = #state{users = Users}) ->
  lager:info("Processing private message..."),
  case lists:keyfind(Username, 1, Users) of
    {Username, Password, _Node}  -> 
      case lists:keyfind(RecipientUsername, 1, Users) of
        {RecipientUsername, _RecipientPassword, RecipientNode} -> 
          process_private_message(Username, RecipientUsername, RecipientNode, Message);
        _ -> 
          lager:warning("Recipient user ~p not found...", [RecipientUsername])
      end;
    false -> lager:warning("Unauthorized public message access attempt for: ~p", [Username])
  end,

  {noreply, State}.

handle_info({auth_client, Username, Password, Node}, State) ->
  {reply, ok, NewState} = handle_call({auth_client, Username, Password, Node}, Node, State),
  {noreply , NewState};
handle_info({process_public_message, Username, Password, Message}, State) ->
  {noreply, NewState} = handle_cast({process_public_message, Username, Password, Message}, State),
  {noreply , NewState};
handle_info({process_private_message, Username, Password, RecipientUserName, Message}, State) ->
  {noreply, NewState} = handle_cast({process_private_message, Username, Password, RecipientUserName, Message}, State),
  {noreply , NewState};
handle_info(_Info, _State) ->
  lager:error("Unhandled message passing to client found!~nINFO:~p~nSTATE:~p", [_Info, _State]),
  {noreply, ok}.

%% ===================================================================
%% Internal methods
%% ===================================================================

process_public_message(Username, Users, Message) ->
  lager:info("[PUBLIC] ~p: ~p~n", [Username, Message]),
  % foldr will take a function and iterate over every entry in the ets database 
  % the following methods are: foldr(fun (Elem::T, IncomingAccumulator) -> outgoingAccumulator, InitialAccumulatorValue, ListToGoThrough :: [T])
  lists:foldr(
    fun(Entry, Acc) ->
      {RecipientUserName, _RecipientPassword, RecipientNode} = Entry,
      case Username of
        RecipientUserName -> Acc; % Do nothing
        _ -> {?CLIENT, RecipientNode} ! {get_message_from_server, Username, Message},
             Acc % Do nothing with the accumulator.
      end
    end,
    0,
    Users
  ).

process_private_message(Username, RecipientUsername, RecipientNode, Message) ->
  {?CLIENT, RecipientNode} ! {get_message_from_server, Username, Message},
  lager:info("[PRIVATE: FROM - ~p] : ~p~n", [Username, Message]),
  lager:info("[PRIVATE: TO - ~p] : ~p~n", [RecipientUsername, Message]).