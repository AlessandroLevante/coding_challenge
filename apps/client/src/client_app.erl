%%%-------------------------------------------------------------------
%% @doc client public API
%% @end
%%%-------------------------------------------------------------------

-module(client_app).

-behaviour(application).

-export([connect/1, start/2, stop/1, test_clients/0]).

start(_StartType, _StartArgs) ->
    client_sup:start_link().

connect(Name) ->
    Sock = connect_client(6789, Name),
    erlang:display("Client started"),
    gen_tcp:close(Sock),
    erlang:display("Connection closed").

% Returns socket
connect_client(Port, Name) ->
    {ok, Sock} = gen_tcp:connect("localhost", Port, [{active, false},
                                                     {packet, 2}]),
    erlang:display("Sending client's name to the socket"),
    gen_tcp:send(Sock, Name),
    Sock.

test_clients() ->
    Sock1 = connect_client(6789, "Client1"),
    Sock2 = connect_client(6789, "Client2"),
    Sock3 = connect_client(6789, "Client3"),
    Sock4 = connect_client(6789, "Client4"),
    Sock5 = connect_client(6789, "Client5"),

    gen_tcp:close(Sock1),
    gen_tcp:close(Sock2),
    gen_tcp:close(Sock3),
    gen_tcp:close(Sock4),
    gen_tcp:close(Sock5).

stop(_State) ->
    ok.

%% internal functions
