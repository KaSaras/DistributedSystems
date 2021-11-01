-module(chat_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % Comment out loglevel in order to show to console all lager output
    % Note, best to comment out all io:format texts as well
    lager:set_loglevel(lager_console_backend, none),
    lager:info("STARTING SERVER APPLICAITON"),
    io:format("Hehe HAHA!~n"),
    chat_server_sup:start_link().

stop(_State) ->
    io:format("Stopping chat server application.~n"),
    lager:info("Stopping chat server application."),
    ok.
