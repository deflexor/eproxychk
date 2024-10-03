%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc eproxychk.

-module(eproxychk).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the eproxychk server.
start() ->
    eproxychk_deps:ensure(),
    ensure_started(crypto),
    eproxychk_mongo:start(),
    eproxychk_mnesia:start(),
    application:start(eproxychk).


%% @spec stop() -> ok
%% @doc Stop the eproxychk server.
stop() ->
    application:stop(eproxychk),
    eproxychk_mnesia:stop(),
    eproxychk_mongo:stop().
