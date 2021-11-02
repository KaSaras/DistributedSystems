-------------------------------- MODULE Chat --------------------------------
EXTENDS Integers,TLC
\* The server node and client node names
CONSTANTS SERVER_NODE, CLIENT_USERNAME, CLIENT_PASSWORD
VARIABLES SERVER_STATE, AUTH_INFO_SET

\* We initialize when we have a server and we have a client started
\* Our initial server state has an empty list of known clients
\* Whereas our client state has the known server node, username and password
\* that we get from the config file when starting up the client
Init == /\ SERVER_STATE = {}
        /\ AUTH_INFO_SET={<<CLIENT_USERNAME, CLIENT_PASSWORD>>}
\* Function that simulates Message passing to a node, doesn't matter what we send, we send it to another node, we assume that messages are always received
ReceiveMessageFromNode == TRUE
\* Function that simulates Message being sent to a node, we assume that messages are always sent
SendMessageToNode == TRUE


\* Authenticate the client. It sends a message, which the server receives
\* Checks if the client username and password exists in the server state
\* If it does, update the server state
\* SINCE TLA doesn't let you drop 
AuthClientExists(auth_info) == \E <<clientNode, clientUsername, clientPassword>> \in SERVER_STATE:
                                    /\ auth_info[1] = clientUsername 
                                    /\ auth_info[2] = clientPassword

RemoveExistingAuthClient(auth_info) == \E authClient \in SERVER_STATE:
                                    (auth_info[2] = authClient[2] /\ auth_info[3] = authClient[3])
                                    
\* For simplicity sake, if there is an existing authClient, we re-add it in the Server state (difficult in TLA+ to update
\* existing value)
Next == \A auth_info \in AUTH_INFO_SET:
            IF AuthClientExists(auth_info)
            THEN /\ SERVER_STATE' = (SERVER_STATE \ auth_info) \union auth_info
            ELSE SERVER_STATE' = SERVER_STATE \union auth_info
        /\  UNCHANGED <<AUTH_INFO_SET>>
                                 
Spec ==  Init /\ [][Next]_<<SERVER_STATE,AUTH_INFO_SET>>

=============================================================================
\* Modification History
\* Last modified Wed Nov 03 01:14:24 EET 2021 by kasaras
\* Created Tue Nov 02 17:19:58 EET 2021 by Sarunas
