-module(chat_client).

%% API
-export([login/0, send_public_message/1, send_private_message/2]).
%% Gen server API
-export([start_link/3]).
%% Gen server callbacks
-export([handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-behavior(gen_server).

-define(CLIENT, chat_client).
-define(SERVER, chat_server).

%% ===================================================================
%% Gen server API and internal methods
%% ===================================================================

start_link(ServerNodeName, Username, Password) ->
  {ok, Pid} = gen_server:start_link(?MODULE, {ServerNodeName, Username, Password}, []),
  register(?CLIENT, Pid),
  lager:info("Started the client genserver: ~p", [{ok, Pid}]),
  lager:info("On node ~p", [node()]),
  {ok, Pid}.

init({ServerNodeName, Username, Password}) ->
  {ok, {ServerNodeName, Username, Password}}.

terminate(_Reason, _State) ->
  ok.

%% ===================================================================
%% Gen server callbacks
%% ===================================================================

handle_call({auth_client}, _From, State = {ServerNodeName, Username, Password}) ->
  lager:info("ServerNodeName: ~p; Username: ~p; Password: ~p", [ServerNodeName, Username, Password]),

  {?SERVER, ServerNodeName} ! {auth_client, Username, Password, node()},
  {reply, ok, State}.

handle_cast({send_public_message, Message}, State = {ServerNodeName, Username, Password}) ->
  {?SERVER, ServerNodeName} ! {process_public_message, Username, Password, Message},
  {noreply, State};

handle_cast({send_private_message, RecipientUserName, Message}, State = {ServerNodeName, Username, Password}) ->
  {?SERVER, ServerNodeName} ! {process_private_message, Username, Password, RecipientUserName, Message},
  {noreply, State};

handle_cast({get_message_from_server, Sender, Message}, State) ->
  lager:info("~s: ~s~n", [Sender, Message]),
  {noreply, State}.  

handle_info({get_message_from_server, Sender, Message}, State) ->
  handle_cast({get_message_from_server, Sender, Message}, State),
  {noreply , State};
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

send_public_message(Message) ->
  gen_server:cast(?CLIENT, {send_public_message, Message}),
  {noreply, ok}.

send_private_message(RecipientUserName, Message) ->
  gen_server:cast(?CLIENT, {send_private_message, RecipientUserName, Message}),
  {noreply, ok}.