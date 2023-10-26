
-module(chat).
-compile(export_all).

init_chat() -> 
    io:format("Waiting for sender ~n"),
    register(rec1, spawn(chat, receive_chat, [])),
    register(connected, spawn(chat, connected, [])).

connected() ->
    receive
        connect_now -> start_chat();
        _ -> connected()
    end.

connect1() ->
    receive
        connect_now -> start_chat1();
        _ -> connect1()
    end.

start_chat() ->
    Name = io:get_line('Enter Your Name: '),
    Name2 = lists:delete($\n, Name),
    [H|T] = nodes(),
    io:format("Hello ~s! Welcome to the chatbox! ~n", [Name2]),
    {connect1, H} ! connect_now,
    cont_chat(Name2,H).


cont_chat(Name,Partner_Node) ->
    Prompt = Name ++ ": ",
    Str = io:get_line(Prompt),
    Str_send = Prompt ++ Str,
    if 
        Str == "bye\n" -> 
            io:format("~n~nYou end the chat...~n"),
            {rec2, Partner_Node} ! bye_now;
        true -> 
            {rec2, Partner_Node} ! Str_send,
            cont_chat(Name,Partner_Node)
    end.

receive_chat() ->
    receive
        bye_now -> io:format("~n~nYour partner ends the chat...~n");
        Mssg -> 
            io:format("~s",[Mssg]),
            receive_chat()
    end.


% ----------------
start_chat1() ->
    Name = io:get_line('Enter Your Name: '),
    Name2 = lists:delete($\n, Name),
    [H|T] = nodes(),
    io:format("Hello ~s! Welcome to the chatbox! ~n", [Name2]),
    cont_chat1(Name2,H).

cont_chat1(Name,Partner_Node) ->
    Prompt = Name ++ ": ",
    Str = io:get_line(Prompt),
    Str_send = Prompt ++ Str,
    if 
        Str == "bye\n" -> 
            io:format("~n~nYou end the chat...~n"),
            {rec1, Partner_Node} ! bye_now;
        true -> 
            {rec1, Partner_Node} ! Str_send,
            cont_chat1(Name,Partner_Node)
    end.



receive_chat1() ->
    receive
        bye_now -> io:format("~n~nYour partner ends the chat...~n");
        Mssg -> 
            io:format("~s",[Mssg]),
            receive_chat1()
    end.

% ----------------

init_chat1(Uid) ->
    net_adm:ping(Uid),
    spawn(chat, confirmConnection, [Uid]).

confirmConnection(Partner_Node) -> 
    {connected, Partner_Node} ! connect_now,
    register(rec2, spawn(chat, receive_chat1, [])),
    register(connect1, spawn(chat, connect1, [])).
