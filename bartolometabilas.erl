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

%   HOW TO RUN !!!
%   
%   Initialize the chat room by running the init_chat function through a host terminal:
%   (Terminal 1 - Host)
%   [1] Head over to the directory where the file is located. (e.g. cd Downloads/bartolometabilas) (folder path)
%   [2] Run Erlang with name and cookie in order to establish a proper connection later on. (erl -name 'v1' -setcookie 'chatroom')
%   [3] Compile the erlang file. (c(bartolometabilas).)
%   [4] Then, simply run the 'init_chat/0' function and wait for another user to connect to the chatroom before beginning the conversation.
%   (bartolometabilas:init_chat().)

%   From another terminal (another user), connect through the aforementioned chatroom by doing the following:
%   [1] Head over to the directory where the file is located. (e.g. cd Downloads/bartolometabilas) (folder path)
%   [2] Run Erlang with a name different from the host terminal and the same cookie (the same with the terminal) in order to establish 
%   a proper connection. (erl -name 'v1' -setcookie 'chatroom')
%   [3] Compile the erlang file. (c(bartolometabilas).)
%   [4] Then, simply run the 'init_chat1/1' function with the host terminal's complete domain as the parameter in the init_chat1(param) function.
%   (DOUBLE CHECK! IT IS INIT_CHAT1(PARAM). NOT JUST INIT_CHAT().) (e.g. bartolometabilas:init_chat1('v1@LAPTOP-DTP9E901.localdomain'))

%   Once the two users have successfully connected to one another:
%   [1] Enter the name for the host terminal first; then
%   [2] Enter the name for the connection terminal.
%   [3] Both of them will be welcomed in the chatroom and they can now begin sending messages to
%   one another without having to wait for the other person's response.
%   [4] If you want to terminate the chatroom, input 'bye' as a message from any of the connected
%   users and the chatroom will be terminated and both users will be disconnected.

%   THAT'S IT! YOU CAN NOW ENJOY HAVING CONVERSATION WITH OTHER PERSON! :>

% --------------------------------------------------------------------------------------------------------------------------

% Modules, imports, and exports
-module(bartolometabilas).
-compile(export_all).

% --------------------------------------------------------------------------------------------------------------------------


% User 1 functions and operations v v v

% Initialize chat function for user one (starts the connection)
init_chat() -> 
    io:format("Waiting for sender ~n"),
    register(rec1, spawn(bartolometabilas, receive_chat, [])),  % Spawns the chat
    register(connected, spawn(bartolometabilas, connected, [])).

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
    spawn(bartolometabilas, confirmConnection, [Uid]). % Spawns the chat with connection to the Uid entered

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
    register(rec2, spawn(bartolometabilas, receive_chat1, [])), 
    register(connect1, spawn(bartolometabilas, connect1, [])).


% --------------------------------------------------------------------------------------------------------------------------

% End

% --------------------------------------------------------------------------------------------------------------------------
