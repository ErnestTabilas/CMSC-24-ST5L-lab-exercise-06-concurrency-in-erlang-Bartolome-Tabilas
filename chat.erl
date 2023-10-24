
-module(chat).
-compile(export_all).

init_chat() -> 
    io:format("Waiting for sender ~n"),
    register(connected, spawn(chat, connected, [])).

connected() ->
    receive
        connect_now -> start_chat();
        _ -> connected()
    end.

start_chat() ->
    Name = io:get_line('Enter Your Name: '),
    Name2 = lists:delete($\n, Name),
    User_Nodes = nodes(),
    [H|T] = User_Nodes,
    io:format("Hello ~s! Welcome to the chatbox! ~n", [Name2]),
    cont_chat(Name2,H).

cont_chat(Name,Partner_Node) ->
    Prompt = Name ++ ": ",
    Str = io:get_line(Prompt),
    Str_send = Prompt ++ Str,
    if 
        Str == "bye\n" -> io:format("~n~nYou Disconnected...~n");
        true -> 
            {rec2, Partner_Node} ! {Str_send, Partner_Node},
            cont_chat(Name,Partner_Node)
    end.

% NOT YET WORKING
% receive_chat() ->
%     receive
%         bye ->
%             io:format("~n~nYour Partner Disconnected...~n") ;
%         { ping , Ping_Pid } ->
%             io:format(" Pong got ping ~n") ,
%             Ping_Pid ! pong ,
%             pong()
%     end.

% user1_receive() ->
%     receive
%         bye -> io:format("~n~nYour Partner Disconnected...~n");
%         {Mssg, Uid1} -> 
%             io:format(Mssg)
%     end.

user2_receive() ->
    receive
        bye -> io:format("~n~nYour Partner Disconnected...~n");
        {Mssg, Uid1} -> 
            io:format("~s",[Mssg]),
            user2_receive()
    end.

init_chat1(Uid) ->
    net_adm:ping(Uid),
    spawn(chat, confirmConnection, [Uid]).

confirmConnection(Partner_Node) -> 
    {connected, Partner_Node} ! connect_now,
    register(rec2, spawn(chat, user2_receive, [])).
