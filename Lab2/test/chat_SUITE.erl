-module(chat_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([test_auth/1,test_broadcast/1]).

%macros for module names, would be more efficient to move them to a .hrl file for reusability
-define(CLIENT, chat_client).
-define(SERVER, chat_server).
-define(NUMBEROFNODES, 20).
-define(TEST_MESSAGE, "Howdy!").

all() ->
    [
        test_auth,
        test_broadcast
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
    
    ServerShortname = server,

    SequenceList = lists:seq(1, ?NUMBEROFNODES),
    ClientConfigList = [
        {
            erlang:list_to_atom("client" ++ erlang:integer_to_list(X)),
            [
                {?CLIENT,
                    [
                        {server, 'server@127.0.0.1'},
                        {username, "Useris_" ++ erlang:integer_to_list(X)},
                        {password, "123"}
                    ]
                }
            ]
        } || X <- SequenceList
    ],
    ct:pal("My list of configs:~p~n",[ClientConfigList]),
    ClientNodeList = lists:foldl(
        fun(Entry, Acc) ->
            {ClientShortName, ClientEnvVars} = Entry,
            {ok, ClientNode} = start_client_node(ClientShortName, ClientEnvVars),
            [ClientNode | Acc]
        end,
        [],
        ClientConfigList
    ),

    {ok, ServerNode} = start_server_node(ServerShortname),
    ct:pal("Client + Server Nodes: ~p, ~p~n",[ClientNodeList, ServerNode]),
    ct:pal("Config ~p~n", [Config]),
    
    [{client_nodes, ClientNodeList}, {server_node, [ServerNode]} | Config].

end_per_suite(Config) ->
    ClientNodes = proplists:get_value(client_nodes,  Config),
    [ServerNode] = proplists:get_value(server_node,  Config),
    
    lists:foreach(fun(Entry) ->
        ok =  rpc:call(Entry, application, stop, [?CLIENT]) 
    end,
    ClientNodes),
    
    ok = rpc:call(ServerNode, application, stop, [chat_server]),
    ok = stop_nodes([ServerNode | ClientNodes]),
    ok.

init_per_testcase(_TestCase, Config) -> 
    % Slave nodes give all output to master (which don't happen quickly enough to be captured by ct:capture_get())
    % We'll focus on the client and server nodes.
    % Meck lets to mock a module
    % Passing the passthrough flag keeps the module's original functionality
    % no_link is used to unlink the process that was used to create the meck
    % Without this, the meck is instantly created and terminated once init_per_testcase finishes.
    ClientNodes = proplists:get_value(client_nodes,  Config),
    [ServerNode] = proplists:get_value(server_node,  Config),
    {ResultList, []} = rpc:multicall(ClientNodes, meck, new, [chat_client, [passthrough, no_link]]),
    [ok] = lists:usort(ResultList),
    ok = rpc:call(ServerNode, meck, new, [chat_server, [passthrough, no_link]]),
    Config.

end_per_testcase(_TestCase, Config) -> 
    ClientNodes = proplists:get_value(client_nodes,  Config),
    [ServerNode] = proplists:get_value(server_node,  Config),
    {ResultList, []} = rpc:multicall(ClientNodes, meck, unload, [?CLIENT]),
    [ok] = lists:usort(ResultList),
    ok = rpc:call(ServerNode, meck, unload, [?SERVER]),
    Config.

test_auth(Config) ->


    SequenceList = lists:seq(1, ?NUMBEROFNODES),
    ServerEResult = [
        {
            "Useris_" ++ erlang:integer_to_list(X),
            "123",
            erlang:list_to_atom("client" ++ erlang:integer_to_list(X) ++ "@127.0.0.1")
        } 
        || X <- SequenceList
    ],

    ClientNodes = proplists:get_value(client_nodes,  Config),
    [ServerNode] = proplists:get_value(server_node,  Config),
    
    lists:foreach(fun(Entry) ->
        {noreply,ok} =  rpc:call(Entry, ?CLIENT, login, []) 
    end,
    ClientNodes),

    % Get the history of executed methods, their arguments and returned values
    rpc:multicall(ClientNodes, meck, history, [?CLIENT]),    
    S = rpc:call(ServerNode, meck, history, [?SERVER]),

    % Using list comprehension + pattern matching to get the returned result of the executed method
    ServerRes = [R || {_, {?SERVER, handle_info, [{auth_client, _, _, _}, _]}, {noreply, {state, R}}} <- S],

    ?assertEqual(lists:sort(ServerEResult),  lists:sort(lists:usort(lists:flatten(ServerRes)))),
    
    Config.

test_broadcast(Config) ->
    ClientNodes = proplists:get_value(client_nodes,  Config),
    SequenceList = lists:seq(1, ?NUMBEROFNODES),
    % ServerEResult = [
    %     {"Useris_" ++ erlang:integer_to_list(X), "123", erlang:list_to_atom("client" ++ erlang:integer_to_list(X) ++ "@127.0.0.1")} 
    %     || X <- SequenceList
    % ],
    % [ServerNode] = proplists:get_value(server_node,  Config),
    MessageHistoryEResult = [
        {
            "Useris_" ++ erlang:integer_to_list(X),
            ?TEST_MESSAGE
        } 
        || X <- SequenceList
    ],

    lists:foreach(fun(Entry) ->
        {noreply,ok} =  rpc:call(Entry, ?CLIENT, login, []) 
    end,
    ClientNodes),

    lists:foreach(fun(Entry) ->
        {noreply,ok} =  rpc:call(Entry, ?CLIENT, get_authorized_clients, []) 
    end,
    ClientNodes),

    lists:foreach(fun(Entry) ->
        {noreply,ok} =  rpc:call(Entry, ?CLIENT, broadcast, [?TEST_MESSAGE]) 
    end,
    ClientNodes),
    % Timer needed due to messages being re-sent between different nodes
    timer:sleep(10000),
    rpc:multicall(ClientNodes, meck, reset, [?CLIENT]),
    {R, _} = rpc:multicall(ClientNodes, ?CLIENT, get_message_history, []),
    ?assertEqual(?NUMBEROFNODES, erlang:length(lists:usort(lists:flatten(R)))),
    
    % Sometimes passes, sometimes fails due to the rebroadcasting not finishing
    lists:foreach(fun(ClientMessageHistory) ->
        ClientMessageHistoryWithoutTimestamp = [
            {Usr, Msg} || {Usr, Msg, _Tmstmp} <- ClientMessageHistory
        ],
        ?assertEqual(lists:sort(MessageHistoryEResult), lists:sort(ClientMessageHistoryWithoutTimestamp)) end,
        R),

    Config.


% ============================================================================
% Helper functions. Taken from the CT test cases used for PAXOID:
% https://github.com/erisata/paxoid/blob/master/test/paxoid_SUITE.erl
% ============================================================================

start_client_node(ClientShortNodeName, EnvVars) ->
    ThisHost = this_host(),
    % ct:pal("Host ~p~n", [ThisHost]),
    {ok, StartedNode} = slave:start(ThisHost, local_name(ClientShortNodeName)),
    % ct:pal("Started Nodes ~p~n", [StartedNode]),
    CodePath = lists:filter(fun filelib:is_dir/1, code:get_path()),
    % ct:pal("Code Path ~p~n", [CodePath]),
    true = rpc:call(StartedNode, code, set_path, [CodePath]),                 % true
    ok = rpc:call(StartedNode, application, set_env, [EnvVars, [{persistent, true}]]),
    {ok, _} = rpc:call(StartedNode, application, ensure_all_started, [chat_client]),  % {ok, _}

    {ok, StartedNode}.

start_server_node(ServerShortNodeName) ->
    ThisHost = this_host(),
    % ct:pal("Host ~p~n", [ThisHost]),
    {ok, StartedNode} = slave:start(ThisHost, local_name(ServerShortNodeName)),
    % ct:pal("Started Nodes ~p~n", [StartedNode]),
    CodePath = lists:filter(fun filelib:is_dir/1, code:get_path()),
    % ct:pal("Code Path ~p~n", [CodePath]),
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