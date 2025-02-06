-module(chat_app_miniclip).
-behaviour(gen_server).

-export([start/1, init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).
-export([create_room/2, destroy_room/2, list_rooms/0, join_room/2, leave_room/2, send_message/3]).

start(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

create_room(Socket, RoomName) ->
    gen_server:call(?MODULE, {create_room, Socket, RoomName}).

destroy_room(Socket, RoomName) ->
    gen_server:call(?MODULE, {destroy_room, Socket, RoomName}).

list_rooms() ->
    gen_server:call(?MODULE, list_rooms).

list_users() ->
    gen_server:call(?MODULE, list_users).

join_room(Socket, RoomName) ->
    gen_server:call(?MODULE, {join_room, Socket, RoomName}).

leave_room(Socket, RoomName) ->
    gen_server:call(?MODULE, {leave_room, Socket, RoomName}).

send_message(Socket, RoomName, Message) ->
    gen_server:cast(?MODULE, {send_message, Socket, RoomName, Message}).

send_private_message(SenderSocket, ReceiverUsername, Message) ->
    gen_server:call(?MODULE, {send_private_message, SenderSocket, ReceiverUsername, Message}).

init([Port]) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,
        {packet, 0},
        {reuseaddr, true},
        {active, true}
    ]),
    spawn(fun() -> accept_connections(ListenSocket) end),
    {ok, #{
        clients => #{},         % Socket => Pid
        rooms => #{},          % RoomName => #{creator => Socket, members => [Socket]}
        client_rooms => #{},    % Socket => [RoomName]
        usernames => #{}        % Usernames => Socket
    }}.

handle_call({send_private_message, SenderSocket, ReceiverUsername, Message}, _From, State) ->
    Usernames = maps:get(usernames, State),
    io:format("Users: ~p~n, user: ~p~n", [Usernames, ReceiverUsername]),
    case maps:find(ReceiverUsername, Usernames) of
        {ok, ReceiverSocket} ->
            % Get sender's username
            Clients = maps:get(clients, State),
            #{SenderSocket := #{username := SenderUsername}} = Clients,

            SenderBinary =
                case SenderUsername of
                    Atom when is_atom(Atom) -> list_to_binary(atom_to_list(Atom));
                    List when is_list(List) -> list_to_binary(List);
                    Bin when is_binary(Bin) -> Bin
                end,
            % Format private message
            PrivateMessage = <<"[Private] ", SenderBinary/binary, ": ", Message/binary, "\n">>,
            
            % Send message to receiver
            gen_tcp:send(ReceiverSocket, PrivateMessage),
            {reply, {ok, sent}, State};
        error ->
            {reply, {error, user_not_found}, State}
    end;

handle_call({create_room, Socket, RoomName}, _From, State) ->
    case maps:is_key(RoomName, maps:get(rooms, State)) of
        true ->
            {reply, {error, room_exists}, State};
        false ->
            NewRooms = maps:put(RoomName, #{creator => Socket, members => [Socket]}, maps:get(rooms, State)),
            NewClientRooms = update_client_rooms(Socket, RoomName, maps:get(client_rooms, State)),
            {reply, {ok, created}, State#{rooms => NewRooms, client_rooms => NewClientRooms}}
    end;

handle_call({destroy_room, Socket, RoomName}, _From, State) ->
    case maps:find(RoomName, maps:get(rooms, State)) of
        {ok, #{creator := Socket}} ->
            Room = maps:get(RoomName, maps:get(rooms, State)),
            notify_room_members(Room, <<"Room ", RoomName/binary, " has been destroyed\n">>),
            
            NewRooms = maps:remove(RoomName, maps:get(rooms, State)),
            NewClientRooms = remove_room_from_all_clients(RoomName, Room, maps:get(client_rooms, State)),
            {reply, {ok, destroyed}, State#{rooms => NewRooms, client_rooms => NewClientRooms}};
        _ ->
            {reply, {error, not_authorized}, State}
    end;

handle_call(list_rooms, _From, State) ->
    Rooms = maps:keys(maps:get(rooms, State)),
    {reply, {ok, Rooms}, State};

handle_call(list_users, _From, State) ->
    Users = maps:keys(maps:get(usernames, State)),
    {reply, {ok, Users}, State};

handle_call({join_room, Socket, RoomName}, _From, State) ->
    case maps:find(RoomName, maps:get(rooms, State)) of
        {ok, Room} ->
            Members = maps:get(members, Room),
            case lists:member(Socket, Members) of
                true ->
                    {reply, {error, already_joined}, State};
                false ->
                    Username = maps:find(Socket , maps:get(clients, State)),

                    NewClients = maps:update_with(Socket, 
                        fun(ClientInfo) -> ClientInfo#{username => Username} end, 
                        #{username => Username}, 
                        maps:get(clients, State)),
                    
                    NewUsernames = maps:put(Username, Socket, maps:get(usernames, State)),
                    
                    NewRoom = Room#{members => [Socket | Members]},
                    NewRooms = maps:put(RoomName, NewRoom, maps:get(rooms, State)),
                    NewClientRooms = update_client_rooms(Socket, RoomName, maps:get(client_rooms, State)),
                    notify_room_members(NewRoom, <<"New user ", Username/binary, " joined the room\n">>),
                    {reply, {ok, joined}, State#{
                        rooms => NewRooms, 
                        client_rooms => NewClientRooms,
                        clients => NewClients,
                        usernames => NewUsernames
                    }}
            end;
        error ->
            {reply, {error, room_not_found}, State}
    end;

handle_call({leave_room, Socket, RoomName}, _From, State) ->
    case maps:find(RoomName, maps:get(rooms, State)) of
        {ok, Room} ->
            Members = maps:get(members, Room),
            case lists:member(Socket, Members) of
                true ->
                    NewMembers = lists:delete(Socket, Members),
                    NewRoom = Room#{members => NewMembers},
                    NewRooms = maps:put(RoomName, NewRoom, maps:get(rooms, State)),
                    NewClientRooms = remove_client_from_room(Socket, RoomName, maps:get(client_rooms, State)),
                    notify_room_members(NewRoom, <<"A user left the room\n">>),
                    {reply, {ok, left}, State#{rooms => NewRooms, client_rooms => NewClientRooms}};
                false ->
                    {reply, {error, not_in_room}, State}
            end;
        error ->
            {reply, {error, room_not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({send_message, Socket, RoomName, Message}, State) ->
    case maps:find(RoomName, maps:get(rooms, State)) of
        {ok, Room} ->
            Members = maps:get(members, Room),
            case lists:member(Socket, Members) of
                true ->
                    % Format message with sender identification (optional)
                    FormattedMessage = <<"Message: ", Message/binary, "\n">>,
                    notify_room_members(Room, FormattedMessage, Socket), % Pass sender Socket
                    {noreply, State};
                false ->
                    {noreply, State}
            end;
        error ->
            {noreply, State}
    end;

handle_cast({register, Socket, Username, Pid}, State) ->
    Clients = maps:put(Socket, #{pid => Pid, username => Username}, maps:get(clients, State)),
    Usernames = maps:put(Username, Socket, maps:get(usernames, State)),
    {noreply, State#{clients => Clients, usernames => Usernames}};

handle_cast({unregister, Socket}, State) ->
    case maps:find(Socket, maps:get(clients, State)) of
        {ok, #{username := Username}} ->
            % Remove from usernames
            NewUsernames = maps:remove(Username, maps:get(usernames, State)),
            
            % Remove from rooms
            NewState = case maps:find(Socket, maps:get(client_rooms, State)) of
                {ok, ClientRooms} ->
                    lists:foldl(
                        fun(RoomName, AccState) ->
                            {reply, _, NewAccState} = handle_call({leave_room, Socket, RoomName}, undefined, AccState),
                            NewAccState
                        end,
                        State#{usernames => NewUsernames},
                        ClientRooms
                    );
                error ->
                    State#{usernames => NewUsernames}
            end,
            
            % Remove from clients
            Clients = maps:remove(Socket, maps:get(clients, NewState)),
            {noreply, NewState#{clients => Clients}};
        error ->
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
accept_connections(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> accept_connections(ListenSocket) end),
    gen_server:cast(?MODULE, {register, Socket, random_username:generate(), self()}),
    client_loop(Socket).

client_loop(Socket) ->
    receive
        {tcp, Socket, Data} ->
            handle_client_message(Socket, Data),
            client_loop(Socket);
        {tcp_closed, Socket} ->
            gen_server:cast(?MODULE, {unregister, Socket});
        {tcp_error, Socket, _} ->
            gen_server:cast(?MODULE, {unregister, Socket})
    end.

parse_command(Data) ->
    % Convert binary to string, trim whitespace and split into parts
    String = string:trim(binary_to_list(Data)),
    Parts = string:split(String, " ", all),
    case Parts of
        ["CREATE", RoomName] ->
            {create_room, list_to_binary(RoomName)};
        ["DESTROY", RoomName] ->
            {destroy_room, list_to_binary(RoomName)};
        ["LIST"] ->
            list_rooms;
        ["PRIVATE", Username | MessageParts] ->
            Message = string:join(MessageParts, " "),
            {send_private_message, list_to_binary(Username), list_to_binary(Message)};
        ["JOIN", RoomName] ->
            {join_room, list_to_binary(RoomName)};
        ["LEAVE", RoomName] ->
            {leave_room, list_to_binary(RoomName)};
        ["USERS"] ->
            list_users;
        ["MSG", RoomName | MessageParts] ->
            Message = string:join(MessageParts, " "),
            {send_message, list_to_binary(RoomName), list_to_binary(Message)};
        ["HELP"] ->
            help;
        _ ->
            {error, invalid_command}
    end.

execute_command(Socket, Command) ->
    case Command of
        {create_room, RoomName} ->
            case create_room(Socket, RoomName) of
                {ok, created} ->
                    gen_tcp:send(Socket, <<"Room created successfully\n">>);
                {error, room_exists} ->
                    gen_tcp:send(Socket, <<"Room already exists\n">>)
            end;

        {destroy_room, RoomName} ->
            case destroy_room(Socket, RoomName) of
                {ok, destroyed} ->
                    gen_tcp:send(Socket, <<"Room destroyed successfully\n">>);
                {error, not_authorized} ->
                    gen_tcp:send(Socket, <<"You are not the room creator\n">>)
            end;

        list_rooms ->
            case list_rooms() of
                {ok, Rooms} ->
                    RoomList = format_room_list(Rooms),
                    gen_tcp:send(Socket, RoomList)
            end;

        list_users ->
            case list_users() of
                {ok, Users} ->
                    UsersList = format_users_list(Users),
                    gen_tcp:send(Socket, UsersList)
            end;

        {join_room, RoomName} ->
            case join_room(Socket, RoomName) of
                {ok, joined} ->
                    gen_tcp:send(Socket, <<"Joined room successfully\n">>);
                {error, already_joined} ->
                    gen_tcp:send(Socket, <<"You are already in this room\n">>);
                {error, room_not_found} ->
                    gen_tcp:send(Socket, <<"Room not found\n">>)
            end;

        {leave_room, RoomName} ->
            case leave_room(Socket, RoomName) of
                {ok, left} ->
                    gen_tcp:send(Socket, <<"Left room successfully\n">>);
                {error, not_in_room} ->
                    gen_tcp:send(Socket, <<"You are not in this room\n">>);
                {error, room_not_found} ->
                    gen_tcp:send(Socket, <<"Room not found\n">>)
            end;

        {send_message, RoomName, Message} ->
            send_message(Socket, RoomName, Message);

      {send_private_message, ReceiverUsername, Message} ->
            case send_private_message(Socket, ReceiverUsername, Message) of
                {ok, sent} ->
                    gen_tcp:send(Socket, <<"Private message sent successfully\n">>);
                {error, user_not_found} ->
                    gen_tcp:send(Socket, <<"User not found\n">>)
            end;

        help ->
            HelpText = get_help_text(),
            gen_tcp:send(Socket, HelpText);

        {error, invalid_command} ->
            gen_tcp:send(Socket, <<"Invalid command. Type HELP for available commands.\n">>)
    end.

%% Helper functions
format_room_list([]) ->
    <<"No rooms available\n">>;
format_room_list(Rooms) ->
    RoomsList = lists:map(
        fun(Room) ->
            [<<"- ">>, Room, <<"\n">>]
        end,
        Rooms
    ),
    list_to_binary([<<"Available rooms:\n">> | RoomsList]).

format_users_list([]) ->
    <<"No rooms available\n">>;
format_users_list(Users) ->
    UsersList = lists:map(
        fun(User) ->
            [<<"- ">>, User, <<"\n">>]
        end,
        Users
    ),
    list_to_binary([<<"Available rooms:\n">> | UsersList]).

get_help_text() ->
    Commands = [
        "Available commands:",
        "CREATE <room_name> - Create a new chat room",
        "DESTROY <room_name> - Destroy a room (creator only)",
        "LIST - Show all available rooms",
        "JOIN <room_name> - Join an existing room",
        "PRIVATE <user> <msg> - Send private message to user",
        "USERS - Show all users",
        "LEAVE <room_name> - Leave a room",
        "MSG <room_name> <message> - Send a message to a room",
        "HELP - Show this help message",
        ""
    ],
    list_to_binary(string:join(Commands, "\n")).

handle_client_message(Socket, Data) ->
    try
        Command = parse_command(Data),
        execute_command(Socket, Command)
    catch
        _:_ ->
            gen_tcp:send(Socket, <<"Error processing command. Type HELP for usage.\n">>)
    end.

notify_room_members(Room, Message) ->
    Members = maps:get(members, Room),
    [gen_tcp:send(Member, Message) || Member <- Members].

notify_room_members(Room, Message, SenderSocket) ->
    Members = maps:get(members, Room),
    [gen_tcp:send(Member, Message) || Member <- Members, Member =/= SenderSocket].

update_client_rooms(Socket, RoomName, ClientRooms) ->
    CurrentRooms = maps:get(Socket, ClientRooms, []),
    maps:put(Socket, [RoomName | CurrentRooms], ClientRooms).

remove_client_from_room(Socket, RoomName, ClientRooms) ->
    case maps:find(Socket, ClientRooms) of
        {ok, Rooms} ->
            NewRooms = lists:delete(RoomName, Rooms),
            maps:put(Socket, NewRooms, ClientRooms);
        error ->
            ClientRooms
    end.

remove_room_from_all_clients(RoomName, Room, ClientRooms) ->
    Members = maps:get(members, Room),
    lists:foldl(
        fun(Socket, Acc) ->
            remove_client_from_room(Socket, RoomName, Acc)
        end,
        ClientRooms,
        Members
    ).
