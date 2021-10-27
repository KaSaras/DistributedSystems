-module(chat_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_auth/1, test_send_public_message/1]).

all() ->
    [
        test_auth,
        test_send_public_message
    ].

init_per_suite(Config) ->

    %application:start(chat_client),
    %application:set_env(an_app, example_key, "value"),
    %application:set_env(an_app, example_key, "value"),
    %application:set_env(an_app, example_key, "value"),
    %application:start(chat_client),
    LocalNodeNames = [
        'server@127.0.0.1',
        'client1@127.0.0.1',
        'client2@127.0.0.1'
    ],
    net_kernel:start('server@127.0.0.1'),
    net_kernel:start('client1@127.0.0.1'),
    application:set_env(chat_client, server, 'server@127.0.0.1'),
    application:set_env(chat_client, username, "Saras"),
    application:set_env(chat_client, password, "123"),
    {ok, _} = application:ensure_all_started(chat_client),
    {ok, _} = application:ensure_all_started(chat_server),
    
    Config.

end_per_suite(_Config) ->
    {ok, _} = applicaiton:stop(chat_client),
    {ok, _} = application:stop(chat_server),
    _Config.

test_auth(_Config) ->
    ?assertEqual(ok, chat_client:login()).
test_send_public_message(_Config) ->
    ?assertEqual(ok, chat_client:send_public_message("Howdy!")).
