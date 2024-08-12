%%%-------------------------------------------------------------------
%% @doc client public API
%% @end
%%%-------------------------------------------------------------------

-module(client_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([connect/1]).
-export([connect_create_room/2, connect_create_print_rooms/3, connect_create_room_receive_messages/2, connect_join_room_receive_messages/2, connect_join_room/2, connect_join_room_send_messages/2, connect_join_leave_room/2, connect_destroy_room/2, connect_create_destroy_room/2, connect_join_leave_wrong_room/3, connect_leave_room/2, connect_create_destroy_wrong_room/3, connect_print_rooms/1]).
-export([connect_create_private_room/2, connect_invite_to_private_room/3]).
-export([receive_message/1]).
-export([connect_send_message/3, connect_send_private_message/3, connect_receive_messages/1]).
-export([test_clients/0, test_rooms/0, test_private_messages/0, test_private_rooms/0]).

-include("server_message_pb.hrl").

start(_StartType, _StartArgs) ->
    client_sup:start_link().

stop(_State) ->
    ok.

% Exported test methods
connect(Name) ->
    connect_client(6789, Name).

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

connect_create_private_room(ClientName, RoomName) ->
    Sock = connect(ClientName),
    create_private_room(Sock, RoomName).

connect_invite_to_private_room(ClientName, RoomName, InvitedClient) ->
    Sock = connect(ClientName),
    send_room_invite(Sock, RoomName, InvitedClient).

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

connect_leave_room(ClientName, RoomName) ->
    Sock = connect(ClientName),
    leave_room(Sock, RoomName).

connect_destroy_room(ClientName, Room) ->
    Sock = connect(ClientName),
    destroy_room(Sock, Room).
    
connect_create_destroy_room(ClientName, Room) ->
    Sock = connect(ClientName),
    create_room(Sock, Room),
    destroy_room(Sock, Room).

connect_create_destroy_wrong_room(ClientName, CreateRoom, DestroyRoom) ->
    Sock = connect(ClientName),
    create_room(Sock, CreateRoom),
    destroy_room(Sock, DestroyRoom).

connect_print_rooms(ClientName) ->
    Sock = connect(ClientName),
    print_rooms(Sock).

connect_send_message(ClientName, Room, Message) ->
    Sock = connect(ClientName),
    send_message(Sock, Room, Message).

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
    create_room(Sock1, "TestRoom1"), % Should be OK
    % Waiting to make sure room has been saved on DynamoDB
    timer:sleep(2000),
    create_room(Sock1, "TestRoom2"), % Should be OK
    % Waiting to make sure room has been saved on DynamoDB
    timer:sleep(2000),
    print_rooms(Sock1), % Should print TestRoom1 and TestRoom2
    destroy_room(Sock1, "TestRoom2"), % Should be OK
    print_rooms(Sock1), % Should print TestRoom1

    Sock2 = connect("Client2"),
    join_room(Sock2, "TestRoom2"), % Should give ERROR
    leave_room(Sock2, "TestRoom2"), % Should give ERROR
    join_room(Sock2, "TestRoom1"), % Should be OK
    destroy_room(Sock2, "TestRoom1"), % Should give ERROR
    print_rooms(Sock1), % Should print TestRoom1
    leave_room(Sock2, "TestRoom1"), % Should be OK
    
    create_room(Sock2, "TestRoom3"), % Should be OK
    % Waiting to make sure room has been saved on DynamoDB
    timer:sleep(2000),
    print_rooms(Sock1), % Should print TestRoom1 and TestRoom3
    destroy_room(Sock2, "TestRoom3"), % Should be OK
    print_rooms(Sock1), % Should print TestRoom1

    Sock3 = connect("Client3"),
    Sock4 = connect("Client4"),
    Sock5 = connect("Client5"),

    join_room(Sock3, "TestRoom1"), % Should be OK
    join_room(Sock4, "TestRoom1"), % Should be OK
    join_room(Sock5, "TestRoom1"), % Should be OK

    send_message(Sock5, "TestRoom2", "Test"), % Should give ERROR
    send_message(Sock2, "TestRoom2", "Test"), % Should give ERROR
    send_message(Sock5, "TestRoom1", "Test 1"), % Should be OK
    send_message(Sock5, "TestRoom1", "Test 2"), % Should be OK
    send_message(Sock5, "TestRoom1", "Test 3"), % Should be OK

    receive_message(Sock1),

    gen_tcp:close(Sock1),
    gen_tcp:close(Sock2),
    gen_tcp:close(Sock3),
    gen_tcp:close(Sock4),
    gen_tcp:close(Sock5).

test_private_messages() ->
    Sock6 = connect("Client6"),
    Sock7 = connect("Client7"),
    timer:sleep(1000), % Waiting to make sure Client7 is connected
    send_private_message(Sock7, "Client8", "Test"), % Should give ERROR
    Sock8 = connect("Client8"),
    Sock9 = connect("Client9"),
    Sock10 = connect("Client10"),
    timer:sleep(1000), % Waiting to make sure all clients are connected

    send_private_message(Sock8, "Client6", "Hi Client6"), % Should be OK
    send_private_message(Sock8, "Client6", "I am Client8"), % Should be OK
    send_private_message(Sock8, "Client6", "I am sending a private message"), % Should be OK

    receive_message(Sock6),

    gen_tcp:close(Sock6),
    gen_tcp:close(Sock7),
    gen_tcp:close(Sock8),
    gen_tcp:close(Sock9),
    gen_tcp:close(Sock10).

test_private_rooms() ->
    Sock11 = connect("Client11"),
    Sock12 = connect("Client12"),
    Sock13 = connect("Client13"),

    create_room(Sock11, "Test2Room1"), % Should be OK
    % Waiting to make sure room has been saved on DynamoDB
    timer:sleep(2000),
    send_room_invite(Sock11, "Test2Room1", "Client12"), % Should give ERROR

    create_private_room(Sock11, "TestPrivateRoom1"), % Should be OK
    % Waiting to make sure room has been saved on DynamoDB
    timer:sleep(2000),
    send_room_invite(Sock11, "Test2Room9", "Client12"), % Should give ERROR
    send_room_invite(Sock13, "TestPrivateRoom1", "Client12"), % Should give ERROR
    send_room_invite(Sock11, "TestPrivateRoom1", "Client12"), % Should be OK

    receive_private_room_invitation(Sock12),

    print_rooms(Sock13), % Should print Test2Room1
    print_rooms(Sock12), % Should print TestPrivateRoom1 and Test2Room1

    join_room(Sock13, "TestPrivateRoom1"), % Should give ERROR
    join_room(Sock12, "TestPrivateRoom1"), % Should be OK

    destroy_room(Sock11, "TestPrivateRoom1"), % Should be OK
    destroy_room(Sock11, "Test2Room1"), % Should be OK

    gen_tcp:close(Sock11),
    gen_tcp:close(Sock12),
    gen_tcp:close(Sock13).

% Utility internal methods
create_room(Sock, RoomName) ->
    ServerMessage = #'ServerMessage'{action = 'CREATE_ROOM', payload = {room, #'Room'{name = RoomName}}},
    gen_tcp:send(Sock, server_message_pb:encode_msg(ServerMessage)).

create_private_room(Sock, RoomName) ->
    ServerMessage = #'ServerMessage'{action = 'CREATE_PRIVATE_ROOM', payload = {room, #'Room'{name = RoomName}}},
    gen_tcp:send(Sock, server_message_pb:encode_msg(ServerMessage)).

join_room(Sock, RoomName) ->
    ServerMessage = #'ServerMessage'{action = 'JOIN_ROOM', payload = {room, #'Room'{name = RoomName}}},
    gen_tcp:send(Sock, server_message_pb:encode_msg(ServerMessage)).

leave_room(Sock, RoomName) ->
    ServerMessage = #'ServerMessage'{action = 'LEAVE_ROOM', payload = {room, #'Room'{name = RoomName}}},
    gen_tcp:send(Sock, server_message_pb:encode_msg(ServerMessage)).

print_rooms(Sock) ->
    ServerMessage = #'ServerMessage'{action = 'LIST_ROOMS'},
    gen_tcp:send(Sock, server_message_pb:encode_msg(ServerMessage)),

    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, ReceivedPacket} ->
            % ReceivedPacket is a protobuf encoded msg
            NewMessage = server_message_pb:decode_msg(ReceivedPacket, 'ServerMessage'),

            case NewMessage#'ServerMessage'.action of
                'LIST_ROOMS' ->
                    ListRoomsProto = element(2, NewMessage#'ServerMessage'.payload),
                    Rooms = ListRoomsProto#'RoomsList'.rooms,
                    io:format("Available rooms:~n", []),
                    [io:format(" - ~s~n", [Room#'Room'.name]) || Room <- Rooms]
            end;
        {error, _} ->
            ok
    end.

destroy_room(Sock, RoomName) ->
    ServerMessage = #'ServerMessage'{action = 'DESTROY_ROOM', payload = {room, #'Room'{name = RoomName}}},
    gen_tcp:send(Sock, server_message_pb:encode_msg(ServerMessage)).

send_message(Sock, RoomName, Message) ->
    ServerMessage = #'ServerMessage'{action = 'SEND_MSG_ROOM', payload = {msgToRoom, #'SendMessageToRoom'{room = #'Room'{name = RoomName}, message = Message}}},
    gen_tcp:send(Sock, server_message_pb:encode_msg(ServerMessage)).

send_private_message(Sock, ClientName, Message) ->
    ServerMessage = #'ServerMessage'{action = 'SEND_PRIVATE_MESSAGE', payload = {sendPrivateMsg, #'SendPrivateMessage'{client = #'Client'{name = ClientName}, message = Message}}},
    gen_tcp:send(Sock, server_message_pb:encode_msg(ServerMessage)).

send_room_invite(Sock, RoomName, ClientName) ->
    ServerMessage = #'ServerMessage'{action = 'SEND_ROOM_INVITE', payload = {invite, #'InviteToRoom'{room = #'Room'{name = RoomName}, client = #'Client'{name = ClientName}}}},
    gen_tcp:send(Sock, server_message_pb:encode_msg(ServerMessage)).

receive_message(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, ReceivedPacket} ->
            % ReceivedPacket is a protobuf encoded msg
            ServerMessage = server_message_pb:decode_msg(ReceivedPacket, 'ServerMessage'),

            case ServerMessage#'ServerMessage'.action of
                'ROOM_MESSAGE' ->
                    RoomMessageProto = element(2, ServerMessage#'ServerMessage'.payload),
                    Sender = RoomMessageProto#'RoomMessage'.sender,
                    Room = RoomMessageProto#'RoomMessage'.roomName,
                    Message = RoomMessageProto#'RoomMessage'.message,

                    io:format("[~s] ~s: ~s~n", [Room, Sender, Message]);
                'PRIVATE_MESSAGE' ->
                    PrivateMessageProto = element(2, ServerMessage#'ServerMessage'.payload),
                    Sender = PrivateMessageProto#'PrivateMessage'.sender,
                    Message = PrivateMessageProto#'PrivateMessage'.message,

                    io:format("[PM] ~s: ~s~n", [Sender, Message]);
                'ROOM_INVITATION' ->
                    RoomInvitationProto = element(2, ServerMessage#'ServerMessage'.payload),
                    Sender = RoomInvitationProto#'RoomInvitation'.sender,
                    Room = RoomInvitationProto#'RoomInvitation'.roomName,

                    io:format("[Invitation] '~s' invited you to join private room '~s'~n", [Sender, Room])
            end;
        {error, _} ->
            ok
    end,
    receive_message(Sock).

receive_private_room_invitation(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, ReceivedPacket} ->
            % ReceivedPacket is a protobuf encoded msg
            ServerMessage = server_message_pb:decode_msg(ReceivedPacket, 'ServerMessage'),

            case ServerMessage#'ServerMessage'.action of
                'ROOM_INVITATION' ->
                    RoomInvitationProto = element(2, ServerMessage#'ServerMessage'.payload),
                    Sender = RoomInvitationProto#'RoomInvitation'.sender,
                    Room = RoomInvitationProto#'RoomInvitation'.roomName,

                    io:format("[Invitation] '~s' invited you to join private room '~s'~n", [Sender, Room])
            end;
        {error, _} ->
            ok
    end.

% Returns socket
connect_client(Port, Name) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary,
                                                     {active, false},
                                                     {packet, 2}]),
    ServerMessage = #'ServerMessage'{action = 'ADD_CLIENT', payload = {'client', #'Client'{name = Name}}},
    gen_tcp:send(Sock, server_message_pb:encode_msg(ServerMessage)),
    Sock.
