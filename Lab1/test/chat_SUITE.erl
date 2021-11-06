-module(chat_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([test_auth/1,test_send_public_message/1, test_send_private_message/1]).

%macros for module names, would be more efficient to move them to a .hrl file for reusability
-define(CLIENT, chat_client).
-define(SERVER, chat_server).

all() ->
    [
        test_auth,
        test_send_public_message,
        test_send_private_message
    ].

init_per_suite(Config) ->
    % To be able to run erlang distributed nodes, epmd has to be launched in -daemon mode
    % otherwise, we won't be able to 
    _ = os:cmd("epmd -daemon"),
    % DOESN'T WORK
    % case node() of
    %     'nonode@nohost' -> net_kernel:start(['127.0.0.1', longnames]);
    %     _               -> ok
    % end,
    % THIS WORKS 
    case node() of
        'nonode@nohost' -> net_kernel:start(['chat_SUITE@127.0.0.1', longnames]);
        _               -> ok
    end,
    {ok, _} = application:ensure_all_started(sasl),
    % ct:pal will print and log in terminal in case test fail
    ct:pal("Starting cihldren nodes...~n"),

    ClientShortname_1 = client1,
    ClientEnvVars_1 = [{?CLIENT, [{server, 'server@127.0.0.1'},{username, "Sarunas"},{password, "123"}]}],
    ClientShortname_2 = client2,
    ClientEnvVars_2 = [{?CLIENT, [{server, 'server@127.0.0.1'},{username, "Testuotojas"},{password, "123"}]}],
    ServerShortname = server,

    {ok, ClientNode1} = start_client_node(ClientShortname_1, ClientEnvVars_1),
    {ok, ClientNode2} = start_client_node(ClientShortname_2, ClientEnvVars_2),
    {ok, ServerNode} = start_server_node(ServerShortname),
    ct:pal("Client + Server Nodes: ~p, ~p, ~p~n",[ClientNode1, ClientNode2, ServerNode]),
    ct:pal("Config ~p~n", [Config]),

    [{client_nodes, [ClientNode1, ClientNode2]}, {server_node, [ServerNode]} | Config].

end_per_suite(Config) ->
    [ClientNode1, ClientNode2] = proplists:get_value(client_nodes,  Config),
    [ServerNode] = proplists:get_value(server_node,  Config),

    ok = rpc:call(ClientNode1, application, stop, [?CLIENT]),
    ok = rpc:call(ClientNode2, application, stop, [?CLIENT]),
    ok = rpc:call(ServerNode, application, stop, [chat_server]),
    ok = stop_nodes([ClientNode1, ClientNode2, ServerNode]),
    ok.

init_per_testcase(_TestCase, Config) -> 
    % Slave nodes give all output to master (which don't happen quickly enough to be captured by ct:capture_get())
    % We'll focus on the client and server nodes.
    % Meck lets to mock a module
    % Passing the passthrough flag keeps the module's original functionality
    % no_link is used to unlink the process that was used to create the meck
    % Without this, the meck is instantly created and terminated once init_per_testcase finishes.
    [ClientNode1, ClientNode2] = proplists:get_value(client_nodes,  Config),
    [ServerNode] = proplists:get_value(server_node,  Config),
    {[ok,ok], []} = rpc:multicall([ClientNode1, ClientNode2], meck, new, [chat_client, [passthrough, no_link]]),
    ok = rpc:call(ServerNode, meck, new, [chat_server, [passthrough, no_link]]),
    Config.

end_per_testcase(_TestCase, Config) -> 
    ClientNodes = proplists:get_value(client_nodes,  Config),
    [ServerNode] = proplists:get_value(server_node,  Config),
    {[ok, ok], []} = rpc:multicall(ClientNodes, meck, unload, [?CLIENT]),
    ok = rpc:call(ServerNode, meck, unload, [?SERVER]),
    Config.

test_auth(Config) ->
    % Expected state after calling the auth_client method
    Client1EResult = {'server@127.0.0.1',"Sarunas","123"},
    Client2EResult = {'server@127.0.0.1',"Testuotojas","123"},
    % Expected server state after authorizing both clients
    ServerEResult = [{"Sarunas","123",'client1@127.0.0.1'},{"Testuotojas","123",'client2@127.0.0.1'}],

    [ClientNode1, ClientNode2] = ClientNodes = proplists:get_value(client_nodes,  Config),
    [ServerNode] = proplists:get_value(server_node,  Config),
    
    {noreply, ok} = rpc:call(ClientNode1, ?CLIENT, login, []),
    {noreply, ok} = rpc:call(ClientNode2, ?CLIENT, login, []),

    % Get the history of executed methods, their arguments and returned values
    {[C1, C2], []} = rpc:multicall(ClientNodes, meck, history, [?CLIENT]),    
    S = rpc:call(ServerNode, meck, history, [?SERVER]),

    % Using list comprehension + pattern matching to get the returned result of the executed method
    [Client1Res] = [R || {_, {?CLIENT, handle_call, [{auth_client}, _, _]}, {reply, ok, R}} <- C1],
    [Client2Res] = [R || {_, {?CLIENT, handle_call, [{auth_client}, _, _]}, {reply, ok, R}} <- C2],
    ServerRes = [R || {_, {?SERVER, handle_info, [{auth_client, _, _, _}, _]}, {noreply, {state, R}}} <- S],

    ?assertEqual(Client1EResult, Client1Res),
    ?assertEqual(Client2EResult, Client2Res),
    ?assertEqual(ServerEResult, lists:usort(lists:flatten(ServerRes))),
    
    Config.

test_send_public_message(Config) ->
    % Client suceeded in executing the method to send a public message
    Client1EResult1 = {'server@127.0.0.1',"Sarunas","123"},
    % Client got the public message from the server
    Client1EResult2 = {get_message_from_server,"Testuotojas","Hi!"},
    Client2EResult1 = {'server@127.0.0.1',"Testuotojas","123"},
    Client2EResult2 = {get_message_from_server,"Sarunas","Howdy!"},

    % Server state after finishing processing messages
    ServerEResult = [{"Sarunas","123",'client1@127.0.0.1'},{"Testuotojas","123",'client2@127.0.0.1'}],
    
    [ClientNode1, ClientNode2] = ClientNodes = proplists:get_value(client_nodes,  Config),
    [ServerNode] = proplists:get_value(server_node,  Config),
    
    {noreply, ok} = rpc:call(ClientNode1, ?CLIENT, send_public_message, ["Howdy!"]),
    {noreply, ok} = rpc:call(ClientNode2, ?CLIENT, send_public_message, ["Hi!"]),
    
    % Unfortunately we'll need to set a 1 second timer
    % This is because the server node sends a message to the client and there may be instances
    % where meck:history() will execute quicker than the server passing the message to a client
    timer:sleep(1000),
    
    % Get history from server node    
    S = rpc:call(ServerNode, meck, history, [?SERVER]),

    % Get server state
    ServerRes = [R || {_, {?SERVER, handle_info, [{process_public_message, _, _, _}, _]}, {noreply, {state, R}}} <- S],
    
    % Get history from client nodes
    {[C1, C2], []} = rpc:multicall(ClientNodes, meck, history, [?CLIENT]),

    % Get their states and messages they received from the server
    [Client1Res1] = [R || {_, {?CLIENT, handle_cast, [{send_public_message, _}, _]}, {noreply, R}} <- C1],
    [Client1Res2] = [R || {_, {?CLIENT, handle_info, [R, _]}, _} <- C1],
    [Client2Res1] = [R || {_, {?CLIENT, handle_cast, [{send_public_message, _}, _]}, {noreply, R}} <- C2],
    [Client2Res2] = [R || {_, {?CLIENT, handle_info, [R, _]}, _} <- C2],

    ?assertEqual(Client1EResult1, Client1Res1),
    ?assertEqual(Client1EResult2, Client1Res2),
    ?assertEqual(Client2EResult1, Client2Res1),
    ?assertEqual(Client2EResult2, Client2Res2),
    ?assertEqual(ServerEResult, lists:usort(lists:flatten(ServerRes))),
    Config.

test_send_private_message(Config) ->
    % Client suceeded in executing the method to send a public message
    Client1EResult1 = {'server@127.0.0.1', "Sarunas", "123"},
    % Client got the private message from the server
    Client1EResult2 = {get_message_from_server, "Testuotojas", "I'm doing great!"},
    Client2EResult1 = {'server@127.0.0.1', "Testuotojas", "123"},
    Client2EResult2 = {get_message_from_server, "Sarunas", "How are you doing?"},
    ServerEResult = [{"Sarunas", "123", 'client1@127.0.0.1'}, {"Testuotojas", "123", 'client2@127.0.0.1'}],
    
    [ClientNode1, ClientNode2] = ClientNodes = proplists:get_value(client_nodes,  Config),
    [ServerNode] = proplists:get_value(server_node,  Config),
    
    {noreply, ok} = rpc:call(ClientNode1, ?CLIENT, send_private_message, ["Testuotojas","How are you doing?"]),
    {noreply, ok} = rpc:call(ClientNode2, ?CLIENT, send_private_message, ["Sarunas","I'm doing great!"]),
    % Unfortunately we'll need to set a 1 second timer
    % This is because the server node sends a message to the client and there may be instances
    % where meck:history() will execute quicker than the server passing the message to a client
    timer:sleep(1000),
    % First get history from server node, since it later sends messages to clients    
    S = rpc:call(ServerNode, meck, history, [?SERVER]),
    ServerRes = [R || {_, {?SERVER, handle_info, [{process_private_message, _, _, _, _}, _]}, {noreply, {state, R}}} <- S],

    % Get history from client nodes
    {[C1, C2], []} = rpc:multicall(ClientNodes, meck, history, [?CLIENT]),

    [Client1Res1] = [R || {_, {?CLIENT, handle_cast, [{send_private_message, _, _}, _]}, {noreply, R}} <- C1],
    [Client1Res2] = [R || {_, {?CLIENT, handle_info, [R, _]}, _} <- C1],
    [Client2Res1] = [R || {_, {?CLIENT, handle_cast, [{send_private_message, _, _}, _]}, {noreply, R}} <- C2],
    [Client2Res2] = [R || {_, {?CLIENT, handle_info, [R, _]}, _} <- C2],

    ?assertEqual(Client1EResult1, Client1Res1),
    ?assertEqual(Client1EResult2, Client1Res2),
    ?assertEqual(Client2EResult1, Client2Res1),
    ?assertEqual(Client2EResult2, Client2Res2),
    ?assertEqual(ServerEResult, lists:usort(lists:flatten(ServerRes))),
    Config.
% ============================================================================
% Helper functions. Taken from the CT test cases used for PAXOID:
% https://github.com/erisata/paxoid/blob/master/test/paxoid_SUITE.erl
% ============================================================================

start_client_node(ClientShortNodeName, EnvVars) ->
    ThisHost = this_host(),
    ct:pal("Host ~p~n", [ThisHost]),
    {ok, StartedNode} = slave:start(ThisHost, local_name(ClientShortNodeName)),
    ct:pal("Started Nodes ~p~n", [StartedNode]),
    CodePath = lists:filter(fun filelib:is_dir/1, code:get_path()),
    ct:pal("Code Path ~p~n", [CodePath]),
    true = rpc:call(StartedNode, code, set_path, [CodePath]),                 % true
    ok = rpc:call(StartedNode, application, set_env, [EnvVars, [{persistent, true}]]),
    {ok, _} = rpc:call(StartedNode, application, ensure_all_started, [chat_client]),  % {ok, _}

    {ok, StartedNode}.

start_server_node(ServerShortNodeName) ->
    ThisHost = this_host(),
    ct:pal("Host ~p~n", [ThisHost]),
    {ok, StartedNode} = slave:start(ThisHost, local_name(ServerShortNodeName)),
    ct:pal("Started Nodes ~p~n", [StartedNode]),
    CodePath = lists:filter(fun filelib:is_dir/1, code:get_path()),
    ct:pal("Code Path ~p~n", [CodePath]),
    true = rpc:call(StartedNode, code, set_path, [CodePath]),                 % true
    {ok, _} = rpc:call(StartedNode, application, ensure_all_started, [chat_server]),  % {ok, _}

    {ok, StartedNode}.

% ============================================================================
% Gets a hostname of the current node.
% ============================================================================
this_host() ->
    % THIS WONT WORK
    % IF your host name has - or _. E.g., mine was Sarunas-ThinkPad-...
    % Erlang will consider this an illegal hostname
    % {ok, ShortHostname} = inet:gethostname(),
    % Gonna use localhost
    ShortHostname = '127.0.0.1',
    {ok, #hostent{h_name = FullHostname}} = inet:gethostbyname(ShortHostname),
    FullHostname.


% ============================================================================
% Extract the local part from the node name.
% ============================================================================
local_name(Node) ->
    case re:run(erlang:atom_to_list(Node), "^(.*?)@", [{capture, all_but_first, list}]) of
        {match, [LocalStr]} -> erlang:list_to_atom(LocalStr);
        nomatch             -> Node
    end.

% ============================================================================
% Filter out the node name
% ============================================================================
node_name(Node) ->
    case re:run(erlang:atom_to_list(Node), "^(.*?)@", [{capture, none}]) of
        match   -> Node;
        nomatch -> erlang:list_to_atom(erlang:atom_to_list(Node) ++ "@" ++ this_host())
    end.

% ============================================================================
% Stop all children nodes
% ============================================================================
stop_nodes(Nodes) ->
    [ ok = slave:stop(Node) || Node <- Nodes],
    ok.