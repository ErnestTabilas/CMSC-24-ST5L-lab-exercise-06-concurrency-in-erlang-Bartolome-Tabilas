
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
    io:format("Hello ~s! Welcome to the chatbox! ~n", [Name2]),
    cont_chat(Name2).

cont_chat(Name) ->
    Prompt = Name ++ ": ",
    Str = io:get_line(Prompt),
    if 
        Str == "bye\n" -> io:format("~n~nYou Disconnected...~n");
        true -> cont_chat(Name)
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

init_chat1(Uid) ->
    net_adm:ping(Uid),
    spawn(chat, confirmConnection, [Uid]).

confirmConnection(Partner_Node) -> 
    {connected, Partner_Node} ! connect_now.



