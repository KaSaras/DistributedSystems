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

%-record(client, {authorized}).

%% ===================================================================
%% Gen server API and internal methods
%% ===================================================================

start_link(ServerNodeName, Username, Password) ->
  {ok, Pid} = gen_server:start_link(?MODULE, {ServerNodeName, Username, Password}, []),
  register(?CLIENT, Pid),
  lager:info("Started the client genserver: ~p", [{ok, Pid}]),
  {ok, Pid}.

init({ServerNodeName, Username, Password}) ->
  {ok, {ServerNodeName, Username, Password}}.

terminate(_Reason, _State) ->
  ok.

%% ===================================================================
%% Gen server callbacks
%% ===================================================================

handle_call({auth_client}, _From, {ServerNodeName, Username, Password}) ->
  lager:info("ServerNodeName: ~p; Username: ~p; Password: ~p", [ServerNodeName, Username, Password]),

  {?SERVER, ServerNodeName} ! {auth_client, Username, Password, node()},
  {reply, ok, {ServerNodeName, Username, Password}}.

handle_cast({send_public_message, Message}, {ServerNodeName, Username, Password}) ->
  {?SERVER, ServerNodeName} ! {process_public_message, Username, Password, Message},
  {noreply, {ServerNodeName, Username, Password}};

handle_cast({send_private_message, RecipientUserName, Message}, {ServerNodeName, Username, Password}) ->
  {?SERVER, ServerNodeName} ! {process_private_message, Username, Password, RecipientUserName, Message},
  {noreply, {ServerNodeName, Username, Password}};

handle_cast({get_message_from_server, Sender, Username, Password, ServerNode, Message}, _State) ->
  lager:info("~s: ~s~n", [Sender, Message]),
  {noreply, {ServerNode, Username, Password}}.  

handle_info({get_message_from_server, Sender, Username, Password, ServerNode, Message}, _State) ->
  spawn(gen_server, cast, [?CLIENT, {get_message_from_server, Sender, Username, Password, ServerNode, Message}]),
  {noreply , ok};
handle_info(_Info, _State) ->
  lager:error("Unhandled message passing to client found!~nINFO:~p~nSTATE:~p", [_Info, _State]),
  {noreply , ok}.

%% ===================================================================
%% Client API
%% ===================================================================

login() ->
  lager:info("LOGING IN..."),
  {ok} = gen_server:call(?CLIENT, {auth_client}),
  lager:info("LOGGED IN!").

send_public_message(Message) ->
  gen_server:cast(?CLIENT, {send_public_message, Message}).

send_private_message(RecipientUserName, Message) ->
  gen_server:cast(?CLIENT, {send_private_message, RecipientUserName, Message}),
  {noreply, ok}.