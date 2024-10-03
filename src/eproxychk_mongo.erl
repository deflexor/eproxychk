%%%-------------------------------------------------------------------
%%% File    : eproxychk_mongo.erl
%%% Author  : dfr
%%% Description : 
%%%
%%% Created : 30 Dec 2011 by dfr
%%%-------------------------------------------------------------------
-module(eproxychk_mongo).
-include("eproxychk.hrl").
-import(eproxychk_util, [read_csv/4]).

-export([update_server/1, insert_server/2, get_servers_for_update/2, get_servers_for_update/3, servers_info/0, get_servers/4, get_servers/3]).
-export([get_settings/1, get_all_settings/0, set_setting/2, get_setting/2, get_setting/1]).
-export([start/0, stop/0]).
-export([load_ip_database/0, country_by_ip/1, get_countries/0]).

-define(DEFINED(F,Default), case F of undefined -> Default; _ -> F end).
-define(DAYS_TO_SECONDS(N), N*24*3600).

%%====================================================================
%% API
%%====================================================================

start() ->
    application:start(emongo),
    emongo:add_pool(mongo_pool, "localhost", 27017, "eproxychk", 50),
    set_default_settings(),
    ok.

stop() ->
    %%application:stop(emongo),
    ok.

%% @spec update_server(Proxy) -> {ok,Pid} | ignore | {error,Error}
%% Proxy = [{K,V}]
%% @doc add or update proxy data
update_server(Proxy) ->
    DocFind = [{"host", ?MAP_VALBB("host", Proxy)},
               {"port", ?MAP_VALBB("port", Proxy)}],
    emongo:update(mongo_pool, "proxyserver",
                  DocFind,
                  ?MAP_UPD(<<"updated">>, eproxychk_util:epoch(), Proxy), true).

%% @spec insert_server(Host,Port) -> 0 | 1
%% @doc insert new server, return 1 if inserted successuffly
insert_server(Host, Port) ->
    DocFind = [{"host", Host},
               {"port", Port}],
    Doc = [{"host", Host},
           {"port", Port},
           {"updated", 0},
           {"stability",0},
           {"enabled",true},
           {"speed",9999},
           {"type","unchecked"},
           {"scans",0},
           {"ok_scans",0}],
    R = emongo:find_all(mongo_pool, "proxyserver", DocFind),
    case R of
        [_] -> 0;
        _ -> emongo:insert(mongo_pool, "proxyserver", Doc),
             1
    end.

%% @spec get_servers_for_update() -> [Proxy]
%% Proxy = [{K,V}]
%% @doc get list of all servers
get_servers_for_update(Limit, Offset) ->
    get_servers_for_update(Limit,Offset, ?DAYS_TO_SECONDS(7)).

get_servers_for_update(Limit, Offset, SecondsAgo) ->
    Expire =  eproxychk_util:epoch() - SecondsAgo,
    emongo:find_all(mongo_pool, "proxyserver",
                    [{"updated", [{'<', Expire}]}, {"enabled", true}],
                    [{limit, Limit}, {offset, Offset}]).

get_servers(Query, Limit, Offset) ->
    get_servers(Query, Limit, Offset, [{"updated", asc}]).

get_servers(Query, Limit, Offset, Sort) ->
    Result = emongo:find_all(mongo_pool, "proxyserver", Query,
                             [{limit, Limit}, {offset, Offset}, {orderby, Sort}]),
    Count = ?DEFINED(emongo:count(mongo_pool, "proxyserver", Query),0),
    {Result, Count}.

servers_info() ->
    Expire =  eproxychk_util:epoch() - ?DAYS_TO_SECONDS(7),
    CntUp = ?DEFINED(emongo:count(mongo_pool, "proxyserver",
                    [{"updated", [{'<', Expire}]}]), 0),
    CntAll = ?DEFINED(emongo:count(mongo_pool, "proxyserver"), 0),
    {CntAll, CntUp}.


%% @spec set_setting(Key,Val) -> {ok,Pid} | ignore | {error,Error}    
set_default_setting(K, V) ->
    case get_setting(K) of
        undefined ->
            set_setting(K,V);
        _ -> ok
    end.

%% @spec set_setting(Key,Val) -> {ok,Pid} | ignore | {error,Error}    
set_setting(K, V) ->
    emongo:update(mongo_pool, "settings",
                  [{"key", K}],
                  [{"key", K},{"val",V}],
                  true).

%% @spec get_settings([Key]) -> [Value]
get_settings(Keys) ->
    lists:map(fun(K) ->
                      R = emongo:find_all(mongo_pool, "settings", [{"key", K}]),
                      case R of
                          [D] -> ?MAP_VAL(<<"val">>, D);
                          _ -> <<"">>
                      end
              end, Keys).

%% @spec get_settings(Key, Default_Value) -> Value
get_setting(Key) ->
    get_setting(Key, undefined).

get_setting(Key, Def) ->
    case get_settings([Key]) of
        [<<"">>] -> Def;
        [B] -> B
    end.

%% @spec get_settings() -> [{Key,Val}]
get_all_settings() ->  emongo:find_all(mongo_pool, "settings").

%% @spec load_ip_database() -> ok
load_ip_database() ->
    DBFile = eproxychk_util:basedir(["priv", ?GEO_DB_FILE]),
    read_csv(DBFile,
             fun () ->
                     emongo:delete(mongo_pool, "geoip"),
                     emongo:delete(mongo_pool, "country")
             end,
             fun([IPFrom,IPTo, Reg, _,Cn,_Cnt,Cntry|_]) ->
                     emongo:insert(mongo_pool, "geoip",
                                   [{"ip_from", list_to_integer(IPFrom)},
                                    {"ip_to", list_to_integer(IPTo)},
                                    {"registry", Reg},
                                    {"cn", Cn},
                                    {"country", Cntry}]),
                     %% countries
                     case Cn /= "ZZ" of
                         true -> emongo:update(mongo_pool, "country", [{"cn", Cn}],
                                               [{"cn", Cn},{"country",Cntry}], true);
                         _ -> ok
                     end
             end,
             fun() ->
                     emongo:ensure_index(mongo_pool, "country", [{"country", 1}])
             end).
            

country_by_ip(Ip) ->
    Ip1 = eproxychk_util:ip_to_int(Ip),
    R = emongo:find_one(mongo_pool, "geoip",
                    [{"ip_from", [{lte, Ip1}]}, {"ip_to", [{gte, Ip1}]}]),
    case R of
        [Doc] -> binary_to_list(?MAP_VAL(<<"cn">>, Doc));
        _ -> "unknown"
    end.
    
get_countries() ->
    emongo:find_all(mongo_pool, "country", [],
                    [{orderby, [{"country", 1}]}]).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
set_default_settings() ->
    set_default_setting("checker_url", "http://murl.mobi/headers.php"),
    set_default_setting("checker_chat", "murl.mobi"),
    set_default_setting("scan_interval", "604800"),
    emongo:ensure_index(mongo_pool, "proxyserver", [{"host", 1}, {"port", 1}]).
    %%set_setting("my_ip", eproxychk_util:my_ip()).

