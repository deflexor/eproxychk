%% @author Mochi Media <dev@mochimedia.com>
%% @copyright eproxychk Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the eproxychk application.

-module(eproxychk_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for eproxychk.
start(_Type, _StartArgs) ->
    eproxychk_deps:ensure(),
    eproxychk_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for eproxychk.
stop(_State) ->
    ok.
