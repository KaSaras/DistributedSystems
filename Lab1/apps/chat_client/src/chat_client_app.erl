-module(chat_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lager:info("STARTING CHAT CLIENT APPLICATION..."),
    Args = [Val || {_Key, Val} <-application:get_all_env()],
    lager:info("USING THE FOLLOWING ENV VARIABLES: ~p",[Args]),
    chat_client_sup:start_link(Args).
    
stop(_State) ->
    ok.
