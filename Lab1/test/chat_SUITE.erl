-module(chat_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_auth/1,test_send_public_message/1, test_send_private_message/1]).

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
    ct:pal("Starting slave nodes...~n"),
    ClientShortname_1 = client1,
    ct:capture_start(),
    ClientEnvVars_1 = [{chat_client, [{server, 'server@127.0.0.1'},{username, "Saras"},{password, "123"}]}],

    ClientShortname_2 = client2,
    ClientEnvVars_2 = [{chat_client, [{server, 'server@127.0.0.1'},{username, "Testeris"},{password, "123"}]}],

    ServerShortname = server,

    {ok, ClientNode1} = start_client_node(ClientShortname_1, ClientEnvVars_1),
    {ok, ClientNode2} = start_client_node(ClientShortname_2, ClientEnvVars_2),
    {ok, ServerNode} = start_server_node(ServerShortname),
    ct:pal("Client + Server Nodes: ~p, ~p, ~p~n",[ClientNode1, ClientNode2, ServerNode]),
    ct:pal("Config ~p~n", [Config]),
    Result = ct:capture_get(),
    ct:pal("Result baby ~p", [Result]),
    [{client_nodes, [ClientNode1, ClientNode2]}, {server_node, [ServerNode]} | Config].

end_per_suite(_Config) ->
    % ok = application:stop(chat_client),
    % ok = application:stop(chat_server),
    _Config.

% Pretty weak test case, since auth method does not take into account failed authentication
% If call returns ok
test_auth(Config) ->
    
    %Arrange
    [ClientNode_1, ClientNode_2] = proplists:get_value(client_nodes,  Config),
    % Slave nodes give all output to master, but we want to focus only on the slave node that has the server
    ct:capture_start(),

    ?assertEqual(ok, rpc:call(ClientNode_1, chat_client, login, [])),
    ?assertEqual(ok, rpc:call(ClientNode_2, chat_client, login, [])),

    % Would capture the terminal output, but the slave node output is not captured
    ct:capture_stop(),
    ok.
test_send_public_message(Config) ->
    [ClientNode_1, ClientNode_2] = proplists:get_value(client_nodes,  Config),
    ?assertEqual(ok, rpc:call(ClientNode_1, chat_client, send_public_message, ["Howdy!"])),
    ?assertEqual(ok, rpc:call(ClientNode_2, chat_client, send_public_message, ["Hi!"])),
    % Same principle, would capture terminal output with ct:capture_start, capture_get and capture_stop, but slave output is not captured
    ok.
test_send_private_message(Config) ->
    [ClientNode_1, ClientNode_2] = proplists:get_value(client_nodes,  Config),
    ?assertEqual({noreply, ok}, rpc:call(ClientNode_1, chat_client, send_private_message, ["Testeris","How are you doing?"])),
    ?assertEqual({noreply, ok}, rpc:call(ClientNode_2, chat_client, send_private_message, ["Saras","I'm doing great!"])),
    % Same principle, would capture terminal output with ct:capture_start, capture_get and capture_stop, but slave output is not captured
    ok.
%%% ============================================================================
%%% Helper functions. Taken from the CT test cases used for PAXOID:
%%% https://github.com/erisata/paxoid/blob/master/test/paxoid_SUITE.erl
%%% ============================================================================

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

%%
%%  Gets a hostname of the current node.
%%
this_host() ->
    % THIS WONT WORK
    % IF your host name has - or _. E.g., mine was Sarunas-ThinkPad-...
    % Erlang will consider this an illegal hostname
    % {ok, ShortHostname} = inet:gethostname(),
    % Gonna use loclahost
    ShortHostname = '127.0.0.1',
    {ok, #hostent{h_name = FullHostname}} = inet:gethostbyname(ShortHostname),
    FullHostname.


%%
%%  Extract the local part from the node name.
%%
local_name(Node) ->
    case re:run(erlang:atom_to_list(Node), "^(.*?)@", [{capture, all_but_first, list}]) of
        {match, [LocalStr]} -> erlang:list_to_atom(LocalStr);
        nomatch             -> Node
    end.

%%
%%
%%
node_name(Node) ->
    case re:run(erlang:atom_to_list(Node), "^(.*?)@", [{capture, none}]) of
        match   -> Node;
        nomatch -> erlang:list_to_atom(erlang:atom_to_list(Node) ++ "@" ++ this_host())
    end.

%%
%%
%%
stop_nodes(Nodes) ->
    [ ok = slave:stop(Node) || Node <- Nodes],
    ok.
