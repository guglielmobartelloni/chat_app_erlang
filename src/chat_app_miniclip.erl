-module(chat_app_miniclip).
-export([start/1, accept_connections/1]).

start(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,
        {packet, 0},
        {reuseaddr, true},
        {active, true}
    ]),
    spawn(?MODULE, accept_connections, [ListenSocket]),
    register(server, self()),
    client_manager().

accept_connections(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, accept_connections, [ListenSocket]),
    handle_client(Socket).

client_manager() ->
    client_manager(#{}).

client_manager(Clients) ->
    receive
        {register, Socket, Pid} ->
            NewClients = Clients#{Socket => Pid},
            client_manager(NewClients);
        {unregister, Socket} ->
            NewClients = maps:remove(Socket, Clients),
            io:format("Disconnecting client ~p \n", [Socket]),
            client_manager(NewClients);
        {broadcast, Socket, Message} ->
            io:format("Message from ~p: ~s", [Socket, Message]),
            maps:foreach(
              % Exclude the sender
                fun(ClientSocket, _Pid) when ClientSocket =/= Socket ->
                    gen_tcp:send(ClientSocket, Message);
                   (_, _) -> ok
            end, Clients),
            client_manager(Clients)
    end.

handle_client(Socket) ->
    server ! {register, Socket, self()},
    client_loop(Socket).

client_loop(Socket) ->
    receive
        {tcp, Socket, Data} ->
            server ! {broadcast, Socket, Data},
            client_loop(Socket);
        {tcp_closed, Socket} ->
            server ! {unregister, Socket};
        {tcp_error, Socket, _} ->
            server ! {unregister, Socket}
    end.
