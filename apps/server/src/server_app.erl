%%%-------------------------------------------------------------------
%% @doc server public API 
%% @end
%%%-------------------------------------------------------------------

-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Starting server...~n"),
    tcp_server:start_link(6789),
    server_sup:start_link().

stop(_State) ->
    io:format("Stopping server...~n"),
    io:format("Server stopped~n"),
    ok.

%% internal functions
