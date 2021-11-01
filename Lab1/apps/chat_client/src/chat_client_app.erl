-module(chat_client_app).

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
    lager:info("STARTING CHAT CLIENT APPLICATION..."),
    % Args = [Val || {_Key, Val} <-application:get_all_env()],
    {ok, ServerNodeName} = application:get_env(server),
    {ok, Username} = application:get_env(username),
    {ok, Password} = application:get_env(password), 
    chat_client_sup:start_link([ServerNodeName, Username, Password]).
    
stop(_State) ->
    io:format("Stopping chat client application.~n"),
    lager:info("Stopping chat client application."),
    ok.
