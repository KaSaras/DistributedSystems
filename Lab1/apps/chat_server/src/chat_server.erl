-module(chat_server).

-export([start_link/0]).
-export([handle_call/3, handle_cast/2, init/1, handle_info/2]).

-behavior(gen_server).

-define(SERVER, chat_server).
-define(CLIENT, chat_client).

start_link() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  register(?SERVER, Pid),
  lager:info("Started the server genserver on:~p~n", [Pid]),
  {ok, Pid}.

handle_call({auth_client, Username, Password, Node}, _From, State) ->
  lager:info("Looking up user in ets..."),
  case ets:lookup(users, Username) of
   [] -> lager:info("Didn't find ~p, inserting new user to the ets table...", [Username]),
         ets:insert(users, {Username, Password, Node});
   [{Username, Password, _Node}] -> lager:info("Existing user ~s has reconnected. Updating their info...", [Username]),
                                    ets:insert(users, {Username, Password, _Node});
    _Else -> io:warning("Somehow, someway, the authentication failed!", [Username])
  end,
  {reply, ok, State}.

handle_cast({process_public_message, Username, Password, Message}, State) ->
  lager:info("Processing public message..."),
  case ets:lookup(users, Username) of
    [{Username, Password, _Node}] -> process_public_message(Username, Message);
    _ -> io:warning("Unauthorized access attempt for: ~p", [Username])
  end,
  {noreply, State};
handle_cast({process_private_message, Username, Password, RecipientUsername, Message}, State) ->
  lager:info("Processing private message..."),
  case ets:lookup(users, Username) of
    [{Username, Password, _Node}] -> 
      case ets:lookup(users, RecipientUsername) of 
        [{RecipientUsername, RecipientPassword, RecipientNode}] -> 
          process_private_message(Username, RecipientUsername, RecipientPassword, RecipientNode, Message);
        _-> lager:warning("Failed to find recipient named ~s in ets table", [RecipientUsername])
      end;
    _Else -> lager:warning("Failed to find user that's trying to send private message: ~s", [Username])
  end,
  {noreply, State}.

handle_info({auth_client, Username, Password, Node}, _State) ->
  spawn(gen_server, call, [?SERVER, {auth_client, Username, Password, Node}]),
  {noreply , ok};
handle_info({process_public_message, Username, Password, Message}, _State) ->
  spawn(gen_server, cast, [?SERVER, {process_public_message, Username, Password, Message}]),
  {noreply , ok};
handle_info({process_private_message, Username, Password, RecipientUserName, Message}, _State) ->
  spawn(gen_server, cast, [?SERVER, {process_private_message, Username, Password, RecipientUserName, Message}]),
  {noreply , ok};
handle_info(_Info, _State) ->
  lager:error("Unhandled message passing to client found!~nINFO:~p~nSTATE:~p", [_Info, _State]),
  {noreply, ok}.

init(_Args) ->
  case ets:info(users) of
    undefined -> ets:new(users, [named_table]);
    _ -> users
  end,
  {ok, null}.

%% Non-api functions

process_public_message(Username, Message) ->
  lager:info("[PUBLIC] ~p: ~p~n", [Username, Message]),
  % foldr will take a function and iterate over every entry in the ets database 
  % the following methods are: foldr(fun (Elem::T, IncomingAccumulator) -> outgoingAccumulator, InitialAccumulatorValue, ListToGoThrough :: [T])
  ets:foldr(
    fun(Entry, Acc) ->
      {RecipientUserName, RecipientPassword, RecipientNode} = Entry,
      case Username of
        RecipientUserName -> Acc; % Do nothing
        _ -> {?CLIENT, RecipientNode} ! {get_message_from_server, Username, RecipientUserName, RecipientPassword, node(), Message},
             Acc % Do nothing with the accumulator.
      end
    end,
    0,
    users
  ).

process_private_message(Username, RecipientUsername, RecipientPassword, RecipientNode, Message) ->
  {?CLIENT, RecipientNode} ! {get_message_from_server, Username, RecipientUsername, RecipientPassword, node(), Message},
  lager:info("[PRIVATE: FROM - ~p] : ~p~n", [Username, Message]),
  lager:info("[PRIVATE: TO - ~p] : ~p~n", [RecipientUsername, Message]).