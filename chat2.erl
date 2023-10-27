% --------------------------------------------------------------------------------------------------------------------------

% Authors: Bartolome, Vallerie
%          Tabilas, Ernest Henley L.
% Section: ST - 5L
% CMSC 124 1st semester 2023-2024

% Exercise 06: Concurrency in Erlang
%   Program description:
%       This is a simple Erlang program that simulates a concurrent two-way
%       chat room between two users from different domains. The user is able
%       message the other user at any given time, without having to wait for
%       the other's response. The chat room is then terminated (both users will
%       be disconnected) once any of the two users uses 'bye' as an input or
%       a message. An individualized/independent set of functions is used for 
%       each user (maximum of two).

% --------------------------------------------------------------------------------------------------------------------------

% Modules, imports, and exports
-module(chat).
-compile(export_all).

% --------------------------------------------------------------------------------------------------------------------------


% User 1 functions and operations v v v

% Initialize chat function for user one (starts the connection)
init_chat() -> 
    io:format("Waiting for sender ~n"),
    register(rec1, spawn(chat, receive_chat, [])),  % Spawns the chat
    register(connected, spawn(chat, connected, [])).

% Helper function to start the chat room
connected() ->
    receive
        connect_now -> start_chat(); % Has established connection
        _ -> connected()
    end.

% Initial function to set the username of the current user once a connection has been established
start_chat() ->
    Name = io:get_line('Enter Your Name: '),
    Name2 = lists:delete($\n, Name), % Name trimmed
    [H|T] = nodes(), % Partner node
    io:format("Hello ~s! Welcome to the chatbox! ~n", [Name2]),
    {connect1, H} ! connect_now, % Notifies the other node about the connection (they can now also enter their name and start the loop/enter the chat room)
    cont_chat(Name2,H). % Enters the recursive loop of accepting message input/enter the chat room

% Recursive function to accept message input from the current user
cont_chat(Name,Partner_Node) ->
    Prompt = Name ++ ": ",
    Str = io:get_line(Prompt),
    Str_send = Prompt ++ Str, % User name with message
    if 
        Str == "bye\n" -> % If the same user inputs 'bye'
            % Exit routine
            io:format("~n~nYou end the chat...~n"),

            % Notifies the other user about the disconnection
            {rec2, Partner_Node} ! Str_send,
            {rec2, Partner_Node} ! bye_now,

            erlang:halt(); % Terminates the chatroom and exits erlang
        true -> % Else
            % Notifies the other user by sending the message
            {rec2, Partner_Node} ! Str_send,
            cont_chat(Name,Partner_Node)
    end.

% Recursive function to receive message from the other user
receive_chat() ->
    receive
        bye_now -> 
            % Exit routine
            io:format("~n~nYour partner ends the chat...~n"),
            erlang:halt(); % Terminates the chatroom and exits erlang
        Mssg -> 
            % Else, simply prints the message received
            io:format("~s",[Mssg]),
            receive_chat()
    end.


% --------------------------------------------------------------------------------------------------------------------------


% User 2 functions and operations v v v

% Initialize chat function for user two (inputs the domain of the user you want to chat with)
init_chat1(Uid) ->
    net_adm:ping(Uid),
    spawn(chat, confirmConnection, [Uid]). % Spawns the chat with connection to the Uid entered

% Helper function to start the chat room
connect1() ->
    receive
        connect_now -> start_chat1(); % Has established connection
        _ -> connect1()
    end.

% Initial function to set the username of the current user once a connection has been established
start_chat1() ->
    Name = io:get_line('Enter Your Name: '),
    Name2 = lists:delete($\n, Name), % Name trimmed
    [H|T] = nodes(), % Partner node
    io:format("Hello ~s! Welcome to the chatbox! ~n", [Name2]),
    cont_chat1(Name2,H).  % Enters the recursive loop of accepting message input/enter the chat room

% Recursive function to accept message input from the current user
cont_chat1(Name,Partner_Node) ->
    Prompt = Name ++ ": ",
    Str = io:get_line(Prompt),
    Str_send = Prompt ++ Str, % User name with message
    if 
        Str == "bye\n" ->   % If the same user inputs 'bye'
            % Exit routine
            io:format("~n~nYou end the chat...~n"),

            % Notifies the other user about the disconnection
            {rec1, Partner_Node} ! Str_send,
            {rec1, Partner_Node} ! bye_now,

            erlang:halt(); % Terminates the chatroom and exits erlang
        true -> % Else
            % Notifies the other user by sending the message
            {rec1, Partner_Node} ! Str_send,
            cont_chat1(Name,Partner_Node)
    end.

% Recursive function to receive message from the other user
receive_chat1() ->
    receive
        bye_now -> 
            % Exit routine
            io:format("~n~nYour partner ends the chat...~n"),
            erlang:halt(); % Terminates the chatroom and exits erlang
        Mssg -> 
            % Else, simply prints the message received
            io:format("~s",[Mssg]),
            receive_chat1()
    end.


% --------------------------------------------------------------------------------------------------------------------------


% Generic functions v v v

% Handshake function to confirm connection between two users
confirmConnection(Partner_Node) -> 
    {connected, Partner_Node} ! connect_now, % Notifies that a connection has been established
    % Counterpart spawnings
    register(rec2, spawn(chat, receive_chat1, [])), 
    register(connect1, spawn(chat, connect1, [])).


% --------------------------------------------------------------------------------------------------------------------------

% End

% --------------------------------------------------------------------------------------------------------------------------
