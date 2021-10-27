-module(chat_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callback
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

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
        chat_server, 
        {
            chat_server,
            start_link,
            []
        },
        ChildRestart,
        Shutdown,
        Type,
        [chat_server]
    }],

    {ok, {SupervisorFlags , ChildSpecificationsList}}.


