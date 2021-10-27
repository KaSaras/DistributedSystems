-module(chat_client_sup).
-behaviour(supervisor).

%% Supervisor API 
-export([start_link/1]).
%% Supervisor Callback
-export([init/1]).

%% ===================================================================
%% Supervisor API
%% ===================================================================
start_link([ChatClient, Username, Password]) ->
    lager:info("STARTING CHAT CLIENT SUPERVISOR...~nClient: ~p UserName: ~p Password: ~p", [ChatClient, Username, Password]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ChatClient, Username, Password]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([ChatClient, Username, Password]) ->

    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 5,
    SupervisorFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    %% permanent - alwas restart
    %% temporary - never restart
    %% transient - restart if abnormally ends
    ChildRestart = permanent,

    %% brutal_kill - use exit(Child, kill) to terminate
    %% integer - use exit(Child, shutdown) - miliseconds
    %% infinity - can be used if chils is supervisor, can be used for worker as wlel,
    %% but introduces race condition - can unlink its children but fail to terminate
    %% them before being killed
    Shutdown = brutal_kill,

    %% worker
    %% supervisor
    Type = worker,

    %% Some child specifications are mandatory, rest are optional
    %% Modules one supervisor uses
    %% ChildSpecifications = {ChildId - mandatory, {StartFunc = {module, function,arg} - mandatory, Restart, Shutdown, Type, Modules}}
    ChildSpecificationsList = [{
        chat_client, 
        {
            chat_client,
            start_link,
            [ChatClient, Username, Password]
        },
        ChildRestart,
        Shutdown,
        Type,
        [chat_client]
    }],

    {ok, {SupervisorFlags , ChildSpecificationsList}}.
