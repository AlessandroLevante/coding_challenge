%%%-------------------------------------------------------------------
%% @doc A tcp server
%% @end
%%%-------------------------------------------------------------------

-module(tcp_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([loop/1, init/1, handle_cast/2]).

start_link(LPort) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    erlang:display("Server started"),
    case gen_tcp:listen(LPort, [{active, false},{packet, 2}]) of
        {ok, ListenSock} ->
            loop_accept(ListenSock);
        {error, Reason} ->
            exit(Reason)
    end.

loop_accept(ListenSock) ->
    {ok, Sock} = gen_tcp:accept(ListenSock),
    erlang:display("A client has connected"),
    Pid = spawn(?MODULE, loop, [Sock]),
    loop_accept(ListenSock).

loop(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Name} ->
            io:format("We received the client name: ~s ~n", [Name]),
            loop(Sock);
        {error, Reason} ->
            ok
  end.

init(Arg) ->
    {ok, {}}.

handle_cast(_msg, State) ->
    {noreply, State}.