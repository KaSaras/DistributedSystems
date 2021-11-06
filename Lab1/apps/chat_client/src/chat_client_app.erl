-module(chat_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % Comment/Uncomment loglevel in order to show to console all lager output
    lager:set_loglevel(lager_console_backend, none),
    % lager:info("STARTING CHAT CLIENT APPLICATION..."),
    % Args = [Val || {_Key, Val} <-application:get_all_env()],
    {ok, ServerNodeName} = application:get_env(server),
    {ok, Username} = application:get_env(username),
    {ok, Password} = application:get_env(password), 
    chat_client_sup:start_link([ServerNodeName, Username, Password]).
    
stop(_State) ->
    lager:info("Stopping chat client application."),
    ok.
