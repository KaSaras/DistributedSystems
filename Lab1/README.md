# LAB 1

## REQUIRED
sudo apt install ssh-askpass

## Important
Due to possible bug that you cannot capture I/O from lager, io:format was added before any meaningful lager output with a duplicate message in order to showcase a meaningful testcase.
Additionally, ct:capture doesn't capture I/O that is sent to master from the slave nodes

## compile code in project directory
~<path_to_project>/ChatServer$ rebar3 compile
## start server node via rebar3 shell
$ rebar3 shell --apps chat_server start --name server@127.0.0.1
## start client1 node via rebar3 shell
$ rebar3 shell --apps chat_client start --name client1@127.0.0.1 --config client.config
(client1@127.0.0.1)1> chat_client:login().
## start client1 node via rebar3 shell
$ rebar3 shell --apps chat_client start --name client2@127.0.0.1 --config client2.config
(client2@127.0.0.1)1> chat_client:login().
## run tests
rebar3 ct
