-module(chat_client).
-export([start/2]).

start(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [
        binary,
        {packet, 0},
        {active, true}
    ]),
    io:format("Connected to server at ~p:~p~n", [Host, Port]),
    
    spawn(fun() -> receiver(Socket) end),
    
    sender(Socket).

receiver(Socket) ->
    receive
        {tcp, Socket, Data} ->
            io:format("Received: ~s", [Data]),
            receiver(Socket);
        {tcp_closed, Socket} ->
            io:format("Connection closed by server~n");
        {tcp_error, Socket, Reason} ->
            io:format("Connection error: ~p~n", [Reason])
    end.

sender(Socket) ->
    case io:get_line("") of
        eof ->
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:format("Error reading input: ~p~n", [Reason]),
            gen_tcp:close(Socket);
        Data ->
            gen_tcp:send(Socket, Data),
            sender(Socket)
    end.
