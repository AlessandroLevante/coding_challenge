%%%-------------------------------------------------------------------
%% @doc server public API 
%% @end
%%%-------------------------------------------------------------------

-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang:display("Starting server..."),
    tcp_server:start_link(6789),
    server_sup:start_link().

stop(_State) ->
    erlang:display("Stopping server..."),
    erlang:display("Server stopped"),
    ok.

%% internal functions
