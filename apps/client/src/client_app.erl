%%%-------------------------------------------------------------------
%% @doc client public API
%% @end
%%%-------------------------------------------------------------------

-module(client_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([connect/1]).
-export([connect_create_room/2, connect_create_print_rooms/3, connect_create_room_receive_messages/2, connect_join_room_receive_messages/2, connect_join_room/2, connect_join_room_send_messages/2, connect_join_leave_room/2, connect_create_destroy_room/2, connect_join_leave_wrong_room/3, connect_create_destroy_wrong_room/3]).
-export([receive_message/1]).
-export([connect_send_private_message/3, connect_receive_messages/1]).
-export([test_clients/0, test_rooms/0, test_private_messages/0]).

start(_StartType, _StartArgs) ->
    client_sup:start_link().

stop(_State) ->
    ok.

% Exported test methods
connect(Name) ->
    Sock = connect_client(6789, Name).

connect_create_room(ClientName, RoomName) ->
    Sock = connect(ClientName),
    create_room(Sock, RoomName).

connect_create_print_rooms(ClientName, RoomName1, RoomName2) ->
    Sock = connect(ClientName),
    create_room(Sock, RoomName1),
    create_room(Sock, RoomName2),
    print_rooms(Sock).

connect_create_room_receive_messages(ClientName, RoomName) ->
    Sock = connect(ClientName),
    create_room(Sock, RoomName),
    receive_message(Sock).

connect_join_room(ClientName, RoomName) ->
    Sock = connect(ClientName),
    join_room(Sock, RoomName).

connect_join_room_send_messages(ClientName, RoomName) ->
    Sock = connect(ClientName),
    join_room(Sock, RoomName),
    send_message(Sock, RoomName, "Test1"),
    send_message(Sock, RoomName, "Test2"),
    send_message(Sock, RoomName, "Test3"),
    send_message(Sock, RoomName, "Heya.").

connect_join_room_receive_messages(ClientName, RoomName) ->
    Sock = connect(ClientName),
    join_room(Sock, RoomName),
    receive_message(Sock).

connect_join_leave_room(ClientName, RoomName) ->
    Sock = connect(ClientName),
    join_room(Sock, RoomName),
    leave_room(Sock, RoomName).

connect_join_leave_wrong_room(ClientName, JoinRoom, LeaveRoom) ->
    Sock = connect(ClientName),
    join_room(Sock, JoinRoom),
    leave_room(Sock, LeaveRoom).
    
connect_create_destroy_room(ClientName, Room) ->
    Sock = connect(ClientName),
    create_room(Sock, Room),
    destroy_room(Sock, Room).

connect_create_destroy_wrong_room(ClientName, CreateRoom, DestroyRoom) ->
    Sock = connect(ClientName),
    create_room(Sock, CreateRoom),
    destroy_room(Sock, DestroyRoom).

connect_send_private_message(ClientName, PMClient, Message) ->
    Sock = connect(ClientName),
    send_private_message(Sock, PMClient, Message).

connect_receive_messages(ClientName) ->
    Sock = connect(ClientName),
    receive_message(Sock).

test_clients() ->
    Sock1 = connect("Client1"),
    Sock2 = connect("Client2"),
    Sock3 = connect("Client3"),
    Sock4 = connect("Client4"),
    Sock5 = connect("Client5"),

    gen_tcp:close(Sock1),
    gen_tcp:close(Sock2),
    gen_tcp:close(Sock3),
    gen_tcp:close(Sock4),
    gen_tcp:close(Sock5).

test_rooms() ->
    Sock1 = connect("Client1"),
    create_room(Sock1, "Room1"), % Should be OK
    create_room(Sock1, "Room2"), % Should be OK
    print_rooms(Sock1), % Should print Room1 and Room2
    destroy_room(Sock1, "Room2"), % Should be OK
    print_rooms(Sock1), % Should print Room1

    Sock2 = connect("Client2"),
    join_room(Sock2, "Room2"), % Should give ERROR
    leave_room(Sock2, "Room2"), % Should give ERROR
    destroy_room(Sock2, "Room1"), % Shouldn't do anything
    print_rooms(Sock1), % Should print Room1
    leave_room(Sock2, "Room1"), % Should be OK
    
    create_room(Sock2, "Room3"), % Should be OK
    print_rooms(Sock1), % Should print Room1 and Room3
    destroy_room(Sock2, "Room3"), % Should be OK
    print_rooms(Sock1), % Should print Room1

    Sock3 = connect("Client3"),
    Sock4 = connect("Client4"),
    Sock5 = connect("Client5"),

    join_room(Sock3, "Room1"), % Should be OK
    join_room(Sock4, "Room1"), % Should be OK
    join_room(Sock5, "Room1"), % Should be OK

    send_message(Sock5, "Room2", "Test"), % Should give ERROR
    send_message(Sock2, "Room2", "Test"), % Should give ERROR
    send_message(Sock5, "Room1", "Test 1"), % Should be OK
    send_message(Sock5, "Room1", "Test 2"), % Should be OK
    send_message(Sock5, "Room1", "Test 3"), % Should be OK

    receive_message(Sock1),

    gen_tcp:close(Sock1),
    gen_tcp:close(Sock2),
    gen_tcp:close(Sock3),
    gen_tcp:close(Sock4),
    gen_tcp:close(Sock5).

test_private_messages() ->
    Sock1 = connect("Client1"),
    Sock2 = connect("Client2"),
    send_private_message(Sock2, "Client3", "Test"), % Should not send anything
    Sock3 = connect("Client3"),
    Sock4 = connect("Client4"),
    Sock5 = connect("Client5"),

    send_private_message(Sock3, "Client1", "Hi Client1"), % Should be OK
    send_private_message(Sock3, "Client1", "I am Client3"), % Should be OK
    send_private_message(Sock3, "Client1", "I am sending a private message"), % Should be OK

    receive_message(Sock1),

    gen_tcp:close(Sock1),
    gen_tcp:close(Sock2),
    gen_tcp:close(Sock3),
    gen_tcp:close(Sock4),
    gen_tcp:close(Sock5).

% Utility internal methods
create_room(Sock, RoomName) ->
    Packet = {create_room, RoomName},
    gen_tcp:send(Sock, term_to_binary(Packet)).

join_room(Sock, RoomName) ->
    Packet = {join_room, RoomName},
    gen_tcp:send(Sock, term_to_binary(Packet)).

leave_room(Sock, RoomName) ->
    Packet = {leave_room, RoomName},
    gen_tcp:send(Sock, term_to_binary(Packet)).

print_rooms(Sock) ->
    Packet = {list_rooms, {}},
    gen_tcp:send(Sock, term_to_binary(Packet)),
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, ReceivedPacket} ->
            Tuple = binary_to_term(ReceivedPacket),
            Type = element(1, Tuple),
            Payload = element(2, Tuple),

            case Type of
                list_rooms ->
                    % Payload is list of rooms
                    io:format("Available rooms:~n", []),
                    [io:format(" - ~s~n", [Room]) || Room <- Payload]
            end;
        {error, Reason} ->
            ok
    end.

destroy_room(Sock, RoomName) ->
    Packet = {destroy_room, RoomName},
    gen_tcp:send(Sock, term_to_binary(Packet)).

send_message(Sock, RoomName, Message) ->
    Packet = {send_msg_room, {RoomName, Message}},
    gen_tcp:send(Sock, term_to_binary(Packet)).

send_private_message(Sock, ClientName, Message) ->
    Packet = {private_message, {ClientName, Message}},
    gen_tcp:send(Sock, term_to_binary(Packet)).

receive_message(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, ReceivedPacket} ->
            Tuple = binary_to_term(ReceivedPacket),
            Type = element(1, Tuple),
            Payload = element(2, Tuple),

            case Type of
                room_message ->
                    Sender = element(1, Payload),
                    Room = element(2, Payload),
                    Message = element(3, Payload),

                    io:format("[~s] ~s: ~s~n", [Room, Sender, Message]);
                private_message ->
                    Sender = element(1, Payload),
                    Message = element(2, Payload),

                    io:format("[PM] ~s: ~s~n", [Sender, Message])
            end;
        {error, Reason} ->
            ok
    end,
    receive_message(Sock).

% Returns socket
connect_client(Port, Name) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary,
                                                     {active, false},
                                                     {packet, 2}]),
    Packet = {connection, Name},
    gen_tcp:send(Sock, term_to_binary(Packet)),
    Sock.
