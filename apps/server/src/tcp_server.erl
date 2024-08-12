%%%-------------------------------------------------------------------
%% @doc A tcp server
%% @end
%%%-------------------------------------------------------------------

-module(tcp_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([loop/2, receiving_thread/2, init/1, handle_cast/2]).

-include("server_message_pb.hrl").

% Records used
-record(client, {name = "", socket}).
-record(room, {name = "", owner = "", type = public, clients = [], invited = []}).

% DynamoDB table names
-define(ROOM_TABLE, <<"Room">>).

% LPort -> Listening port of the server
start_link(LPort) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    % Configuring DynamoDB
    erlcloud_ddb2:configure(
        "FAKEKEY",
        "FAKEPASS",
        "localhost",
        9000,
        "http://"
    ),
    % Getting previously saved rooms
    Rooms = get_rooms_from_ddb(),
    % This thread will manage requests for data shared between the clients
    RecvThreadPid = spawn(?MODULE, receiving_thread, [[], Rooms]),
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
    spawn(?MODULE, loop, [Sock, RecvThreadPid]),
    loop_accept(ListenSock, RecvThreadPid).

receiving_thread(Clients, Rooms) ->
    % Configuring DynamoDB again, since this method gets executed in another thread
    erlcloud_ddb2:configure(
        "FAKEKEY",
        "FAKEPASS",
        "localhost",
        9000,
        "http://"
    ),
    receive
        {add_client, ClientSocket, ClientName} ->
            Client = #client{name = ClientName, socket = ClientSocket},
            io:format("Client '~s' connected~n", [Client#client.name]),
            % Adding Client in Clients list
            receiving_thread([Client | Clients], Rooms);
        {create_room, ClientSocket, RoomName} ->
            receiving_thread(Clients, create_room(Rooms, Clients, ClientSocket, RoomName));
        {destroy_room, ClientSocket, RoomName} ->
            receiving_thread(Clients, remove_room(Rooms, Clients, RoomName, ClientSocket));
        {list_rooms, ClientSocket, ClientPid} ->
            % Send message back with rooms list
            ClientPid ! {list_rooms, get_rooms_list(Clients, Rooms, ClientSocket)};
        {join_room, ClientSocket, RoomName} ->
            receiving_thread(Clients, add_client_to_room(Rooms, Clients, ClientSocket, RoomName));
        {leave_room, ClientSocket, RoomName} ->
            receiving_thread(Clients, remove_client_from_room(Rooms, Clients, ClientSocket, RoomName));
        {send_msg_room, ClientSocket, RoomName, Message} ->
            send_message_to_room(Rooms, Clients, ClientSocket, Message, RoomName);
        {private_message, ClientSocket, ClientName, Message} ->
            send_private_message(Clients, ClientSocket, Message, ClientName);
        {create_private_room, ClientSocket, RoomName} ->
            receiving_thread(Clients, create_private_room(Rooms, Clients, ClientSocket, RoomName));
        {send_room_invite, ClientSocket, RoomName, ClientName} ->
            receiving_thread(Clients, send_room_invite(Rooms, Clients, ClientSocket, RoomName, ClientName))
    end,
    receiving_thread(Clients, Rooms).

loop(Sock, ServerPid) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Packet} ->
            % Packet is a tuple, first element is the Type of request, the second is the Payload
            %Tuple = binary_to_term(Packet),
            %Type = element(1, Tuple),
            %Payload = element(2, Tuple),

            % Packet is a protobuf encoded msg
            ServerMessage = server_message_pb:decode_msg(Packet, 'ServerMessage'),

            case ServerMessage#'ServerMessage'.action of
                'ADD_CLIENT' ->
                    % Payload is client
                    ClientProto = element(2, ServerMessage#'ServerMessage'.payload),
                    ServerPid ! {add_client, Sock, ClientProto#'Client'.name};
                'CREATE_ROOM' ->
                    % Payload is room
                    RoomProto = element(2, ServerMessage#'ServerMessage'.payload),
                    ServerPid ! {create_room, Sock, RoomProto#'Room'.name};
                'DESTROY_ROOM' ->
                    % Payload is room
                    RoomProto = element(2, ServerMessage#'ServerMessage'.payload),
                    ServerPid ! {destroy_room, Sock, RoomProto#'Room'.name};
                'LIST_ROOMS' ->
                    % Empty payload
                    % Passing this thread's pid to receive the list back
                    ServerPid ! {list_rooms, Sock, self()},
                    % Waiting to receive list
                    receive
                        {list_rooms, Rooms} ->
                            ListRoomsSvrMsg = #'ServerMessage'{action = 'LIST_ROOMS', payload = {rooms, #'RoomsList'{rooms = Rooms}}},
                            % Sending list of available rooms to client
                            gen_tcp:send(Sock, server_message_pb:encode_msg(ListRoomsSvrMsg))
                    end;
                'JOIN_ROOM' ->
                    % Payload is room
                    RoomProto = element(2, ServerMessage#'ServerMessage'.payload),
                    ServerPid ! {join_room, Sock, RoomProto#'Room'.name};
                'LEAVE_ROOM' ->
                    % Payload is room
                    RoomProto = element(2, ServerMessage#'ServerMessage'.payload),
                    ServerPid ! {leave_room, Sock, RoomProto#'Room'.name};
                'SEND_MSG_ROOM' ->
                    % Payload contains room and message to send
                    SendMsgRoomProto = element(2, ServerMessage#'ServerMessage'.payload),
                    RoomName = SendMsgRoomProto#'SendMessageToRoom'.room#'Room'.name,
                    Message = SendMsgRoomProto#'SendMessageToRoom'.message,
                    ServerPid ! {send_msg_room, Sock, RoomName, Message};
                'SEND_PRIVATE_MESSAGE' ->
                    % Payload contains client and message to send
                    PrivateMsgProto = element(2, ServerMessage#'ServerMessage'.payload),
                    ClientName = PrivateMsgProto#'SendPrivateMessage'.client#'Client'.name,
                    Message = PrivateMsgProto#'SendPrivateMessage'.message,
                    ServerPid ! {private_message, Sock, ClientName, Message};
                'CREATE_PRIVATE_ROOM' ->
                    % Payload is room
                    RoomProto = element(2, ServerMessage#'ServerMessage'.payload),
                    ServerPid ! {create_private_room, Sock, RoomProto#'Room'.name};
                'SEND_ROOM_INVITE' ->
                    % Payload contains room and client
                    RoomInviteProto = element(2, ServerMessage#'ServerMessage'.payload),
                    RoomName = RoomInviteProto#'InviteToRoom'.room#'Room'.name,
                    ClientName = RoomInviteProto#'InviteToRoom'.client#'Client'.name,
                    ServerPid ! {send_room_invite, Sock, RoomName, ClientName}
            end,
            loop(Sock, ServerPid);
        {error, _} ->
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
    [io:format(" - ~s~n", [Client]) || Client <- Room#room.clients].

print_room_participants(Room) ->
    io:format("~s participants:~n", [Room#room.name]),
    [io:format(" - ~s~n", [Client]) || Client <- Room#room.clients].

% --- DynamoDB methods ---
% -- Get data from DynamoDB --
get_rooms_from_ddb() ->
    Result = erlcloud_ddb_util:scan_all(?ROOM_TABLE),
    case Result of
        {ok, Items} ->
            lists:map(fun map_ddb_item_to_room/1, Items);
        {error, _} ->
            []
    end.

get_messages_from_ddb(Room) ->
    Result = erlcloud_ddb2:get_item(?ROOM_TABLE, get_key(Room)),
    case Result of
        {ok, Items} ->
            map_ddb_item_to_messages(Items);
        {error, _} ->
            []
    end.

% Adds a room in room table on DynamoDB
add_room_ddb(Room) ->
    Items = map_room_to_ddb_items(Room),
    erlcloud_ddb2:put_item(?ROOM_TABLE, Items).

% Adds a message in a room in room table on DynamoDB
add_room_message_ddb(Room, Sender, Message) ->
    PreviousMessages = get_messages_from_ddb(Room),
    Items = map_message_to_ddb_items(Sender, Message, PreviousMessages),
    erlcloud_ddb2:update_item(?ROOM_TABLE, get_key(Room), Items).

% Updates a room in room table on DynamoDB
update_room_ddb(Room) ->
    Items = map_room_to_ddb_items_update(Room),
    erlcloud_ddb2:update_item(?ROOM_TABLE, get_key(Room), Items).

% Deletes a room from room table on DynamoDB
delete_room_ddb(Room) ->
    erlcloud_ddb2:delete_item(?ROOM_TABLE, get_key(Room)).

% -- DynamoDB utilities --
% Gets the key (name) for Room table
get_key(Room) ->
    [ {<<"name">>, {s, list_to_binary(Room#room.name)}} ].

% -- Map data from and to DynamoDB --
map_ddb_item_to_room(Items) ->
    Name = binary_to_list(proplists:get_value(<<"name">>, Items)),
    Owner = binary_to_list(proplists:get_value(<<"owner">>, Items)),
    Type = binary_to_atom(proplists:get_value(<<"type">>, Items)),
    Clients = [binary_to_list(Client) || Client <- proplists:get_value(<<"clients">>, Items)],
    
    case Type of
        public ->
            #room{name = Name, owner = Owner, type = Type, clients = Clients, invited = []};
        private ->
            Invited = [binary_to_list(Client) || Client <- proplists:get_value(<<"invited">>, Items)],
            #room{name = Name, owner = Owner, type = Type, clients = Clients, invited = Invited}
    end.

map_ddb_item_to_messages(Items) ->
    case proplists:is_defined(<<"messages">>, Items) of
        true ->
            [binary_to_term(Message) || Message <- proplists:get_value(<<"messages">>, Items)];
        false ->
            []
    end.

map_room_to_ddb_items(Room) ->
    case Room#room.type of
        public ->
            [
                {<<"name">>, list_to_binary(Room#room.name)},
                {<<"owner">>, list_to_binary(Room#room.owner)},
                {<<"type">>, {b, atom_to_binary(Room#room.type)}},
                {<<"clients">>, {ss, [list_to_binary(Client) || Client <- Room#room.clients]}}
            ];
        private ->
            [
                {<<"name">>, list_to_binary(Room#room.name)},
                {<<"owner">>, list_to_binary(Room#room.owner)},
                {<<"type">>, {b, atom_to_binary(Room#room.type)}},
                {<<"clients">>, {ss, [list_to_binary(Client) || Client <- Room#room.clients]}},
                {<<"invited">>, {ss, [list_to_binary(Client) || Client <- Room#room.invited]}}
            ]
    end.

map_room_to_ddb_items_update(Room) ->
    case Room#room.type of
        public ->
            [
                {<<"owner">>, list_to_binary(Room#room.owner), put},
                {<<"type">>, {b, atom_to_binary(Room#room.type)}, put},
                {<<"clients">>, {ss, [list_to_binary(Client) || Client <- Room#room.clients]}, put}
            ];
        private ->
            [
                {<<"owner">>, list_to_binary(Room#room.owner), put},
                {<<"type">>, {b, atom_to_binary(Room#room.type)}, put},
                {<<"clients">>, {ss, [list_to_binary(Client) || Client <- Room#room.clients]}, put},
                {<<"invited">>, {ss, [list_to_binary(Client) || Client <- Room#room.invited]}, put}
            ]
    end.

map_message_to_ddb_items(Sender, Message, PreviousMessages) ->
    Messages = [{Sender, Message} | PreviousMessages],
    [
        {<<"messages">>, {bs, [term_to_binary(MessageToMap) || MessageToMap <- Messages]}, put}
    ].

% --- End DynamoDB methods ---

% -- Utility methods for managing records --
% Filters Rooms list, maintaining only entries with the passed name
% Returns first element of the filtered list
get_room_by_name(Rooms, RoomName) ->
    % Check if room exists
    case lists:any(fun(Room) -> Room#room.name == RoomName end, Rooms) of
        true -> 
            [Room | _] = lists:filter(fun(Room) -> Room#room.name == RoomName end, Rooms),
            Room;
        false -> ok
    end.

% Filters Clients list, maintaining only entries with the passed socket
% Returns first element of the filtered list
get_client_from_socket(Clients, ClientSocket) ->
    % Check if client is connected
    case lists:any(fun(Client) -> Client#client.socket == ClientSocket end, Clients) of
        true -> 
            [Client | _] = lists:filter(fun(Client) -> Client#client.socket == ClientSocket end, Clients),
            Client;
        false -> io:format("[ERROR] [~p] This client is not connected~n", [?FUNCTION_NAME])
    end.

% Filters Clients list, maintaining only entries with the passed name
% Returns first element of the filtered list
get_client_by_name(Clients, ClientName) ->
    % Check if a client with this name exists
    case lists:any(fun(Client) -> Client#client.name == ClientName end, Clients) of
        true -> 
            [Client | _] = lists:filter(fun(Client) -> Client#client.name == ClientName end, Clients),
            Client;
        false -> ok
    end.

% Returns the list of rooms containing the new room if the creation is successful
create_room(Rooms, Clients, ClientSocket, RoomName) ->
    % Checking if room already exist
    ExistingRoom = get_room_by_name(Rooms, RoomName),
    case not is_record(ExistingRoom, room) of
        true ->
            Client = get_client_from_socket(Clients, ClientSocket),
            Room = #room{name = RoomName, owner = Client#client.name, clients = [Client#client.name]},
            Response = add_room_ddb(Room),
            case Response of
                {ok, _} ->
                    io:format("Created room '~s'~n", [RoomName]),
                    % Adding new room to rooms list
                    [Room | Rooms];
                {error, Error} ->
                    ErrorWhere = element(1, Error),
                    ErrorType = element(2, Error),
                    io:format("[ERROR] [~p] Some error occured while adding new room to DynamoDB: ~s - ~s~n", [?FUNCTION_NAME, ErrorWhere, ErrorType]),
                    Rooms
            end;
        false ->
            % If ExistingRoom is not a record it means a room with the passed name doesn't exist
            io:format("[ERROR] [~p] Room '~s' already exists~n", [?FUNCTION_NAME, RoomName]),
            Rooms
    end.

% Returns the list of rooms containing the new room if the creation is successful
create_private_room(Rooms, Clients, ClientSocket, RoomName) ->
    % Checking if room already exist
    ExistingRoom = get_room_by_name(Rooms, RoomName),
    case not is_record(ExistingRoom, room) of
        true -> 
            Client = get_client_from_socket(Clients, ClientSocket),
            Room = #room{name = RoomName, owner = Client#client.name, type = private, clients = [Client#client.name], invited = [Client#client.name]},
            Response = add_room_ddb(Room),
            case Response of
                {ok, _} ->
                    io:format("Created private room '~s'~n", [RoomName]),
                    % Adding new room to rooms list
                    [Room | Rooms];
                {error, Error} ->
                    ErrorWhere = element(1, Error),
                    ErrorType = element(2, Error),
                    io:format("[ERROR] [~p] Some error occured while adding new room to DynamoDB: ~s - ~s~n", [?FUNCTION_NAME, ErrorWhere, ErrorType]),
                    Rooms
            end;
        false ->
            % If ExistingRoom is not a record it means a room with the passed name doesn't exist
            io:format("[ERROR] [~p] Room '~s' already exists~n", [?FUNCTION_NAME, RoomName]),
            Rooms
    end.

% Returns true if room must be removed
% Room must be removed if passed name is related to a room owned by the caller
should_room_be_removed(Room, RoomName, Owner) ->
    (Room#room.owner == Owner) andalso (Room#room.name == RoomName).

% Returns the list of rooms without the rooms that got removed
remove_room(Rooms, Clients, RoomName, ClientSocket) ->
    Client = get_client_from_socket(Clients, ClientSocket),
    case lists:any(fun(Room) -> should_room_be_removed(Room, RoomName, Client#client.name) end, Rooms) of
        true ->
            Room = get_room_by_name(Rooms, RoomName),
            Response = delete_room_ddb(Room),
            case Response of
                {ok, _} ->
                    io:format("Removed room '~s'~n", [RoomName]),
                    % Filter creates a list with the elements that respect the given condition,
                    % so rooms that should not be removed will be returned
                    lists:filter(fun(RoomToRemove) -> not should_room_be_removed(RoomToRemove, RoomName, Client#client.name) end, Rooms);
                {error, Error} ->
                    ErrorWhere = element(1, Error),
                    ErrorType = element(2, Error),
                    io:format("[ERROR] [~p] Some error occured while adding new room to DynamoDB: ~s - ~s~n", [?FUNCTION_NAME, ErrorWhere, ErrorType]),
                    Rooms
            end;
        false ->
            io:format("[ERROR] [~p] Error removing room ~s~n", [?FUNCTION_NAME, RoomName]),
            Rooms
    end.

get_clients_room(Room) ->
    [#'Client'{name = Client} || Client <- Room#room.clients].

% Returns a list containing only rooms to send to client
get_rooms_list(Clients, Rooms, ClientSocket) ->
    Client = get_client_from_socket(Clients, ClientSocket),
    [#'Room'{name = Room#room.name, clients = get_clients_room(Room)} || Room <- Rooms, (Room#room.type == public) or ((Room#room.type == private) and (lists:member(Client#client.name, Room#room.invited)))].

% Returns the list of rooms, adding the client that wants to connect to the room
add_client_to_room(Rooms, Clients, ClientSocket, RoomName) ->
    Room = get_room_by_name(Rooms, RoomName),
    % Checking if room exists
    case is_record(Room, room) of
        true ->
            Client = get_client_from_socket(Clients, ClientSocket),
            % Checking if room is public or private
            case Room#room.type of
                public ->
                    NewRoom = connect_client_to_room(Room, Client),
                    Response = update_room_ddb(NewRoom),
                    case Response of
                        {ok, _} ->
                            send_previous_messages(Client, NewRoom),
                            % Creating new list that contains all rooms different from Room, adding NewRoom
                            % Basically we are replacing Room with NewRoom
                            [NewRoom | [R || R <- Rooms, R /= Room]];
                        {error, Error} ->
                            ErrorWhere = element(1, Error),
                            ErrorType = element(2, Error),
                            io:format("[ERROR] [~p] Some error occured while updating room on DynamoDB: ~s - ~s~n", [?FUNCTION_NAME, ErrorWhere, ErrorType]),
                            Rooms
                    end;
                private ->
                    case lists:member(Client#client.name, Room#room.invited) of
                        true ->
                            NewRoom = connect_client_to_room(Room, Client),
                            send_previous_messages(Client, NewRoom),
                            % Creating new list that contains all rooms different from Room, adding NewRoom
                            % Basically we are replacing Room with NewRoom
                            [NewRoom | [R || R <- Rooms, R /= Room]];
                        false ->
                            io:format("[ERROR] [~p] Client '~s' has not been invited to room '~s'~n", [?FUNCTION_NAME, Client#client.name, RoomName]),
                            Rooms
                    end
            end;
        false ->
            % Room doesn't exist, nothing changes
            io:format("[ERROR] [~p] Room '~s' doesn't exist~n", [?FUNCTION_NAME, RoomName]),
            Rooms
    end.

% Returns the room adding a new connected client
connect_client_to_room(Room, Client) ->
    % If the client is not a member of this room
    case not lists:member(Client#client.name, Room#room.clients) of 
        true ->
            io:format("Added client '~s' to room '~s'~n", [Client#client.name, Room#room.name]),
            % Updating room clients
            Room#room{clients = [Client#client.name | Room#room.clients]};
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
            Response = update_room_ddb(NewRoom),
            case Response of
                {ok, _} ->
                    % Creating new list that contains all rooms different from Room, adding NewRoom
                    % Basically we are replacing Room with NewRoom
                    [NewRoom | [R || R <- Rooms, R /= Room]];
                {error, Error} ->
                    ErrorWhere = element(1, Error),
                    ErrorType = element(2, Error),
                    io:format("[ERROR] [~p] Some error occured while updating room on DynamoDB: ~s - ~s~n", [?FUNCTION_NAME, ErrorWhere, ErrorType]),
                    Rooms
            end;
        false ->
            % Room doesn't exist, nothing changes
            io:format("[ERROR] [~p] Room '~s' doesn't exist~n", [?FUNCTION_NAME, RoomName]),
            Rooms
    end.

% Returns the the room deleting the disconnected client
disconnect_client_from_room(Room, Client) ->
    % If the client is a member of this room
    case lists:member(Client#client.name, Room#room.clients) of 
        true ->
            io:format("Removing client '~s' from room '~s'~n", [Client#client.name, Room#room.name]),
            Room#room{clients = lists:delete(Client#client.name, Room#room.clients)};
        false ->
            % Client is not part of the room, nothing changes
            io:format("[ERROR] [~p] Client '~s' is not part of the room '~s'~n", [?FUNCTION_NAME, Client#client.name, Room#room.name]),
            Room
    end.

create_message_to_room_proto_record(Sender, RoomName, Message) ->
    #'ServerMessage'{action = 'ROOM_MESSAGE', payload = {roomMsg, #'RoomMessage'{sender = Sender, roomName = RoomName, message = Message}}}.

send_message_to_room(Rooms, Clients, ClientSocket, Message, RoomName) ->
    Room = get_room_by_name(Rooms, RoomName),
    % Checking if room exists
    case is_record(Room, room) of
        true ->
            Client = get_client_from_socket(Clients, ClientSocket),
            % If the client is a member of this room
            case lists:member(Client#client.name, Room#room.clients) of
                true ->
                    % Sends the message to every room participant
                    ClientsToSend = [get_client_by_name(Clients, RoomParticipant) || RoomParticipant <- Room#room.clients],
                    ServerMessage = create_message_to_room_proto_record(Client#client.name, Room#room.name, Message),
                    add_room_message_ddb(Room, Client#client.name, Message),
                    [gen_tcp:send(ClientToSend#client.socket, server_message_pb:encode_msg(ServerMessage)) || ClientToSend <- ClientsToSend];
                false ->
                    io:format("[ERROR] [~p] Client '~s' is not part of the room '~s'~n", [?FUNCTION_NAME, Client#client.name, Room#room.name])
            end;
        false ->
            io:format("[ERROR] [~p] Room '~s' doesn't exist~n", [?FUNCTION_NAME, RoomName])
    end.

% Returns the room adding the newly invited client
send_private_message(Clients, ClientSocket, Message, ClientName) ->
    Receiver = get_client_by_name(Clients, ClientName),
    % Checking if client exists
    case is_record(Receiver, client) of
        true ->
            Sender = get_client_from_socket(Clients, ClientSocket),
            ServerMessage = #'ServerMessage'{action = 'PRIVATE_MESSAGE', payload = {privateMsg, #'PrivateMessage'{sender = Sender#client.name, message = Message}}},
            gen_tcp:send(Receiver#client.socket, server_message_pb:encode_msg(ServerMessage));
        false ->
            io:format("[ERROR] [~p] Client '~s' doesn't exist~n", [?FUNCTION_NAME, ClientName])
    end.

send_previous_messages(Client, Room) ->
    Messages = get_messages_from_ddb(Room),
    [gen_tcp:send(Client#client.socket, server_message_pb:encode_msg(create_message_to_room_proto_record(element(1, Message), Room#room.name, element(2, Message)))) || Message <- Messages].

send_room_invite(Rooms, Clients, ClientSocket, RoomName, ClientName) ->
    Room = get_room_by_name(Rooms, RoomName),
    Client = get_client_from_socket(Clients, ClientSocket),
    % Checking if room exists
    case is_record(Room, room) of
        true ->
            % Checking if caller is the owner of the room
            case Room#room.owner == Client#client.name of
                true ->
                    % Checking if room is public or private
                    case Room#room.type of
                        private ->
                            InvitedClient = get_client_by_name(Clients, ClientName),
                            case is_record(InvitedClient, client) of
                                true ->
                                    % Checking if client is not already invited
                                    case not lists:member(InvitedClient#client.name, Room#room.invited) of
                                        true ->
                                            % Send message to client
                                            ServerMessage = #'ServerMessage'{action = 'ROOM_INVITATION', payload = {roomInvitation, #'RoomInvitation'{sender = Client#client.name, roomName = RoomName}}},
                                            gen_tcp:send(InvitedClient#client.socket, server_message_pb:encode_msg(ServerMessage)),
                                            NewRoom = Room#room{invited = [InvitedClient#client.name | Room#room.invited]},
                                            Response = update_room_ddb(NewRoom),
                                            case Response of
                                                {ok, _} ->
                                                    % Add new room to list
                                                    [NewRoom | [R || R <- Rooms, R /= Room]];
                                                {error, Error} ->
                                                    ErrorWhere = element(1, Error),
                                                    ErrorType = element(2, Error),
                                                    io:format("[ERROR] [~p] Some error occured while updating room on DynamoDB: ~s - ~s~n", [?FUNCTION_NAME, ErrorWhere, ErrorType]),
                                                    Rooms
                                            end;
                                            
                                        false ->
                                            io:format("[ERROR] [~p] Client '~s' has already been invited to room '~s'~n", [?FUNCTION_NAME, Client#client.name, RoomName]),
                                            Rooms
                                    end;
                                false ->
                                    io:format("[ERROR] [~p] Client '~s' doesn't exist~n", [?FUNCTION_NAME, Client#client.name]),
                                    Rooms
                            end;
                        public ->
                            io:format("[ERROR] [~p] Clients cannot be invited to public rooms~n", [?FUNCTION_NAME]),
                            Rooms
                    end;
                false ->
                    io:format("[ERROR] [~p] Client '~s' is not the owner of room '~s'~n", [?FUNCTION_NAME, Client#client.name, RoomName]),
                    Rooms
            end;
        false ->
            io:format("[ERROR] [~p] Room '~s' doesn't exist~n", [?FUNCTION_NAME, RoomName]),
            Rooms
    end.

init(_) ->
    process_flag(trap_exit, true),
    {ok, {}}.

handle_cast(_msg, State) ->
    {noreply, State}.