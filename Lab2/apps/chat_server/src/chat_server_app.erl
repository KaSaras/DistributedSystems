-module(chat_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % Comment/Uncomment loglevel in order to show to console all lager output
    % lager:set_loglevel(lager_console_backend, none),
    lager:info("STARTING SERVER APPLICAITON"),
    chat_server_sup:start_link().

stop(_State) ->
    lager:info("Stopping chat server application."),
    ok.
