%%%-------------------------------------------------------------------
%% @doc A tcp server
%% @end
%%%-------------------------------------------------------------------

-module(tcp_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([loop/2, receiving_thread/2, init/1, handle_cast/2]).

% Records used
-record(client, {name = "", socket}).
-record(room, {name = "", owner, clients = []}).  

% LPort -> Listening port of the server
start_link(LPort) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    % This thread will manage requests for data shared between the clients
    RecvThreadPid = spawn(?MODULE, receiving_thread, [[], []]),
    io:format("TCP server started~n"),
    case gen_tcp:listen(LPort, [binary, {active, false},{packet, 2}]) of
        {ok, ListenSock} ->
            loop_accept(ListenSock, RecvThreadPid);
        {error, Reason} ->
            exit(Reason)
    end.

loop_accept(ListenSock, RecvThreadPid) ->
    {ok, Sock} = gen_tcp:accept(ListenSock),
    % This thread gets spawned for every client connections, manages requests
    ClientPid = spawn(?MODULE, loop, [Sock, RecvThreadPid]),
    loop_accept(ListenSock, RecvThreadPid).

receiving_thread(Clients, Rooms) ->
    receive
        {add_client, ClientSocket, ClientName} ->
            Client = #client{name = ClientName, socket = ClientSocket},
            io:format("Client '~s' connected~n", [Client#client.name]),
            % Adding Client in Clients list
            receiving_thread([Client | Clients], Rooms);
        {create_room, ClientSocket, RoomName} ->
            receiving_thread(Clients, create_room(Rooms, Clients, ClientSocket, RoomName));
        {destroy_room, ClientSocket, RoomName} ->
            io:format("Trying to remove room '~s'...~n", [RoomName]),
            receiving_thread(Clients, remove_room(Rooms, RoomName, ClientSocket));
        {list_rooms, ClientPid} ->
            % Send message back with rooms list
            ClientPid ! {list_rooms, get_rooms_list(Rooms)};
        {join_room, ClientSocket, RoomName} ->
            receiving_thread(Clients, add_client_to_room(Rooms, Clients, ClientSocket, RoomName));
        {leave_room, ClientSocket, RoomName} ->
            receiving_thread(Clients, remove_client_from_room(Rooms, Clients, ClientSocket, RoomName));
        {send_msg_room, ClientSocket, RoomName, Message} ->
            send_message_to_room(Rooms, Clients, ClientSocket, Message, RoomName)
    end,
    receiving_thread(Clients, Rooms).

loop(Sock, ServerPid) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Packet} ->
            % Packet is a tuple, first element is the Type of request, the second is the Payload
            Tuple = binary_to_term(Packet),
            Type = element(1, Tuple),
            Payload = element(2, Tuple),

            case Type of
                connection ->
                    % Payload is client name
                    ServerPid ! {add_client, Sock, Payload};
                create_room ->
                    % Payload is room name
                    ServerPid ! {create_room, Sock, Payload};
                destroy_room ->
                    % Payload is room name
                    ServerPid ! {destroy_room, Sock, Payload};
                list_rooms ->
                    % Empty payload
                    % Passing this thread's pid to receive the list back
                    ServerPid ! {list_rooms, self()},
                    % Waiting to receive list
                    receive
                        {list_rooms, Rooms} ->
                            % Sending list of available rooms to client
                            gen_tcp:send(Sock, term_to_binary({list_rooms, Rooms}))
                    end;
                join_room ->
                    % Payload is room name
                    ServerPid ! {join_room, Sock, Payload};
                leave_room ->
                    % Payload is room name
                    ServerPid ! {leave_room, Sock, Payload};
                send_msg_room ->
                    % Payload is a tuple containing room name and message to send
                    RoomName = element(1, Payload),
                    Message = element(2, Payload),
                    ServerPid ! {send_msg_room, Sock, RoomName, Message}
            end,
            loop(Sock, ServerPid);
        {error, Reason} ->
            ok
    end.

% Utility methods to print the status of the server
print_clients(Clients) ->
    io:format("Connected clients:~n"),
    [io:format(" - ~s~n", [Client#client.name]) || Client <- Clients]. 

print_rooms(Rooms) ->
    io:format("Available rooms:~n"),
    [io:format(" - ~s~n", [Room#room.name]) || Room <- Rooms].

print_rooms_and_participants(Rooms) ->
    [print_room_participants(Rooms, Room#room.name) || Room <- Rooms].

print_room_participants(Rooms, RoomName) ->
    Room = get_room_by_name(Rooms, RoomName),
    io:format("~s participants:~n", [Room#room.name]),
    [io:format(" - ~s~n", [Client#client.name]) || Client <- Room#room.clients].

print_room_participants(Room) ->
    io:format("~s participants:~n", [Room#room.name]),
    [io:format(" - ~s~n", [Client#client.name]) || Client <- Room#room.clients].

% Filters Rooms list, maintaining only entries with the passed name
% Returns first element of the filtered list
get_room_by_name(Rooms, RoomName) ->
    % Check if room exists
    case lists:any(fun(Room) -> Room#room.name == RoomName end, Rooms) of
        true -> 
            [Room | Rest] = lists:filter(fun(Room) -> Room#room.name == RoomName end, Rooms),
            Room;
        false -> ok
    end.

% Filters Clients list, maintaining only entries with the passed socket
% Returns first element of the filtered list
get_client_from_socket(Clients, ClientSocket) ->
    % Check if client is connected
    case lists:any(fun(Client) -> Client#client.socket == ClientSocket end, Clients) of
        true -> 
            [Client | Rest] = lists:filter(fun(Client) -> Client#client.socket == ClientSocket end, Clients),
            Client;
        false -> io:format("[ERROR] [~p] This client is not connected~n", [?FUNCTION_NAME])
    end.

% Returns the list of rooms containing the new room if the creation is successful
create_room(Rooms, Clients, ClientSocket, RoomName) ->
    % Checking if room already exist
    ExistingRoom = get_room_by_name(Rooms, RoomName),
    case not is_record(ExistingRoom, room) of
        true -> 
            Room = #room{name = RoomName, owner = ClientSocket, clients = [get_client_from_socket(Clients, ClientSocket)]},
            io:format("Created room '~s'~n", [RoomName]),
            % Adding new room to rooms list
            [Room | Rooms];
        false ->
            % If ExistingRoom is not a record it means a room with the passed name doesn't exist
            io:format("[ERROR] [~p] Room '~s' already exists~n", [?FUNCTION_NAME, RoomName]),
            Rooms
    end.

% Returns true if room must be removed
% Room must be removed if passed name is related to a room owned by the caller
should_room_be_removed(Room, RoomName, ClientSocket) ->
    (Room#room.owner == ClientSocket) andalso (Room#room.name == RoomName).

% Returns the list of rooms without the rooms that got removed
remove_room(Rooms, RoomName, ClientSocket) ->
    % Filter creates a list with the elements that respect the given condition,
    % so rooms that should not be removed will be in NewRooms list
    NewRooms = lists:filter(fun(Room) -> not should_room_be_removed(Room, RoomName, ClientSocket) end, Rooms).

% Returns a list containing only the rooms' names
get_rooms_list(Rooms) ->
    [Room#room.name || Room <- Rooms].

% Returns the list of rooms, adding the client that wants to connect to the room
add_client_to_room(Rooms, Clients, ClientSocket, RoomName) ->
    Room = get_room_by_name(Rooms, RoomName),
    % Checking if room exists
    case is_record(Room, room) of
        true -> 
            Client = get_client_from_socket(Clients, ClientSocket),
            NewRoom = connect_client_to_room(Room, Client),
            % Creating new list that contains all rooms different from Room, adding NewRoom
            % Basically we are replacing Room with NewRoom
            [NewRoom | [R || R <- Rooms, R /= Room]];
        false ->
            % Room doesn't exist, nothing changes
            io:format("[ERROR] [~p] Room '~s' doesn't exist~n", [?FUNCTION_NAME, RoomName]),
            Rooms
    end.

% Returns the the room adding a new connected client
connect_client_to_room(Room, Client) ->
    % If the client is not a member of this room
    case not lists:member(Client, Room#room.clients) of 
        true ->
            io:format("Added client '~s' to room '~s'~n", [Client#client.name, Room#room.name]),
            % Updating room clients
            Room#room{clients = [Client | Room#room.clients]};
        false ->
            io:format("[ERROR] [~p] Client '~s' is already in room '~s'~n", [?FUNCTION_NAME, Client#client.name, Room#room.name]),
            Room
    end.

% Returns the list of rooms, removing the client that wants to disconnect to the room
remove_client_from_room(Rooms, Clients, ClientSocket, RoomName) ->
    Room = get_room_by_name(Rooms, RoomName),
    % Checking if room exists
    case is_record(Room, room) of
        true -> 
            Client = get_client_from_socket(Clients, ClientSocket),
            NewRoom = disconnect_client_from_room(Room, Client),
            % Creating new list that contains all rooms different from Room, adding NewRoom
            % Basically we are replacing Room with NewRoom
            [NewRoom | [R || R <- Rooms, R /= Room]];
        false ->
            % Room doesn't exist, nothing changes
            io:format("[ERROR] [~p] Room '~s' doesn't exist~n", [?FUNCTION_NAME, RoomName]),
            Rooms
    end.

% Returns the the room deleting the disconnected client
disconnect_client_from_room(Room, Client) ->
    % If the client is a member of this room
    case lists:member(Client, Room#room.clients) of 
        true ->
            io:format("Removing client '~s' from room '~s'~n", [Client#client.name, Room#room.name]),
            Room#room{clients = lists:delete(Client, Room#room.clients)};
        false ->
            % Client is not part of the room, nothing changes
            io:format("[ERROR] [~p] Client '~s' is not part of the room '~s'~n", [?FUNCTION_NAME, Client#client.name, Room#room.name]),
            Room
    end.

send_message_to_room(Rooms, Clients, ClientSocket, Message, RoomName) ->
    Room = get_room_by_name(Rooms, RoomName),
    % Checking if room exists
    case is_record(Room, room) of
        true ->
            Client = get_client_from_socket(Clients, ClientSocket),
            % If the client is a member of this room
            case lists:member(Client, Room#room.clients) of
                true ->
                    % Sends the message to every room participant
                    [gen_tcp:send(RoomParticipant#client.socket, term_to_binary({room_message, {Client#client.name, Room#room.name, Message}})) || RoomParticipant <- Room#room.clients];
                false ->
                    io:format("[ERROR] [~p] Client '~s' is not part of the room '~s'~n", [?FUNCTION_NAME, Client#client.name, Room#room.name])
            end;
        false ->
            io:format("[ERROR] [~p] Room '~s' doesn't exist~n", [?FUNCTION_NAME, RoomName])
    end.

init(Arg) ->
    process_flag(trap_exit, true),
    {ok, {}}.

handle_cast(_msg, State) ->
    {noreply, State}.