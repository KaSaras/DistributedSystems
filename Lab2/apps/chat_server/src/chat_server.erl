-module(chat_server).

-export([start_link/0]).
-export([handle_call/3, init/1, handle_info/2]).

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
handle_cast({get_authorized_clients, Username, Password}, State = #state{users = Users}) ->
  case lists:keyfind(Username, 1, Users) of
    {Username, Password, Node}  -> ok = send_authorized_clients(Node, Users);
    false -> 
      lager:warning("Unauthorized public message access attempt for: ~p", [Username])
  end,  
  {reply, ok, State}.

handle_info({auth_client, Username, Password, Node}, State) ->
  {reply, ok, NewState} = handle_call({auth_client, Username, Password, Node}, Node, State),
  {noreply , NewState};
handle_info({get_authorized_clients, Username, Password}, State) ->
  {reply, ok, NewState} = handle_cast({get_authorized_clients, Username, Password}, State),
  {noreply, NewState}.
%% ===================================================================
%% Internal methods
%% ===================================================================
send_authorized_clients(Node, Users) ->
  UsersWithoutPassword = [{Username, Nodename} || {Username, _, Nodename} <- Users],
  {?CLIENT, Node} ! {get_authorized_clients_from_server, UsersWithoutPassword},
  ok.