-module(chat_app_miniclip).
-behaviour(gen_server).

-export([start/1, init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

%% API
start(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%% GenServer Callbacks
init([Port]) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,
        {packet, 0},
        {reuseaddr, true},
        {active, true}
    ]),
    spawn(fun() -> accept_connections(ListenSocket) end),
    {ok, #{clients => #{}}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({register, Socket, Pid}, State) ->
    Clients = maps:put(Socket, Pid, maps:get(clients, State)),
    {noreply, State#{clients => Clients}};

handle_cast({unregister, Socket}, State) ->
    Clients = maps:remove(Socket, maps:get(clients, State)),
    io:format("Disconnecting client ~p \n", [Socket]),
    {noreply, State#{clients => Clients}};

handle_cast({broadcast, Socket, Message}, State) ->
    io:format("Message from ~p: ~s", [Socket, Message]),
    Clients = maps:get(clients, State),
    maps:foreach(
        fun(ClientSocket, _Pid) when ClientSocket =/= Socket ->
            gen_tcp:send(ClientSocket, Message);
           (_, _) -> ok
        end, Clients),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Accept Connections
accept_connections(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> accept_connections(ListenSocket) end),
    gen_server:cast(?MODULE, {register, Socket, self()}),
    client_loop(Socket).

client_loop(Socket) ->
    receive
        {tcp, Socket, Data} ->
            gen_server:cast(?MODULE, {broadcast, Socket, Data}),
            client_loop(Socket);
        {tcp_closed, Socket} ->
            gen_server:cast(?MODULE, {unregister, Socket});
        {tcp_error, Socket, _} ->
            gen_server:cast(?MODULE, {unregister, Socket})
    end.
