# LAB 1

## REQUIRED
sudo apt install ssh-askpass (believe it's for setting up the shortname@longname)

## Important
When performing the CT tests, it was not possible to capture console I/O of children nodes with ct:capture_get() due to processes calling multiple operations. Because of this, Meck was used in order to be able to check what method was called by the server, their arguments and returned values (1 sec timer was used as well because server node sends messages back to the clients).
## Client config files
Client config file holds 3 values: the name of the servernode (atom), client's username (string), client's password (string)
## compile code in project directory
~<path_to_project>/Lab1$ rebar3 compile
## start server node via rebar3 shell
$ rebar3 shell --apps chat_server start --name server@127.0.0.1
## start client1 node via rebar3 shell
$ rebar3 shell --apps chat_client start --name client1@127.0.0.1 --config client.config
## start client2 node via rebar3 shell
$ rebar3 shell --apps chat_client start --name client2@127.0.0.1 --config client2.config
## authenticate client
(client1@127.0.0.1)1> chat_client:login().
## send public message 
(client1@127.0.0.1)1> chat_client:send_public_message("MESSAGE").
## send private message 
(client1@127.0.0.1)1> chat_client:send_private_message("RECIPIENT_USER_NAME", "MESSAGE").
## run tests
rebar3 ct