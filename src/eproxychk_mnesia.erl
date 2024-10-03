%%%-------------------------------------------------------------------
%%% File    : eproxychk_mnesia.erl
%%% Author  : dfr
%%% Description : mnesia storage layer TODO: ?
%%%
%%% Created : 30 Dec 2011 by dfr
%%%-------------------------------------------------------------------
-module(eproxychk_mnesia).
-include_lib("stdlib/include/qlc.hrl").
%%-include_lib("stdlib/include/ms_transform.hrl").
-include("eproxychk.hrl").
-import(eproxychk_util, [read_csv/4]).

-export([update_server/1, add_server/1, get_settings/1, get_settings/0, set_settings/1, start/0, stop/0, get_servers/0 ]).
-export([load_ip_database/0, country_by_ip/1, get_countries/0]).

-define(MERGE_PR(R, StatR),
        R#proxyserver{
          scans = StatR#proxyserver.scans + R#proxyserver.scans,
          ok_scans = StatR#proxyserver.ok_scans + R#proxyserver.ok_scans
         }).
-define(NO_KEY(K, S), proplists:get_value(K, S) == undefined).

%%====================================================================
%% API
%%====================================================================

start() ->
    io:format("starting mnesia: create schema~n",[]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:wait_for_tables([geoip, country], 5000),
    set_defaults(),
    ok.

stop() ->
    mnesia:stop(),
    ok.

%% @spec update_server(Proxy) -> {ok,Pid} | ignore | {error,Error}
%% Proxy = #proxyserver
%% @doc add or update proxy data
update_server(#proxyserver{} = Proxy) ->
    T = fun() ->
                [Pold] = mnesia:wread({ proxyserver, Proxy#proxyserver.host }),
                Pnew = ?MERGE_PR(Proxy, Pold),
                mnesia:write(Pnew)
        end,
    mnesia:transaction(T).

%% @spec add_server(Proxy#proxyserver) -> 0 | 1
%% @doc add proxy server if not exist, returns 1 if succesfull
add_server(#proxyserver{} = Proxy) ->
    T = fun() ->
                case mnesia:wread({ proxyserver, Proxy#proxyserver.host }) of
                    [_] -> 0;
                    _ ->
                        mnesia:write(Proxy),
                        1
                end
        end,
    case mnesia:transaction(T) of
        {atomic, R} -> R;
        {aborted, Reason} -> error("add_server tx abort: " ++ Reason)
    end.

%% @spec get_servers() -> [Server#proxyserver]
%% @doc get list of all servers
get_servers() ->
    Q = qlc:q([E || E <- mnesia:table(proxyserver)]),
    qlc:e(Q).


%% @spec set_settings([{Key,Val}]) -> {ok,Pid} | ignore | {error,Error}    
set_settings(Settings) ->
    T = fun () ->
                WriteFun = fun ({K,V}) ->
                                   mnesia:write(#settings{key = K, val = V})
                           end,
                lists:foreach(WriteFun, Settings)
        end,
    mnesia:transaction(T).

%% @spec get_settings([Key]) -> [Value]
get_settings(Keys) ->
    lists:map(fun (K) ->
                      case mnesia:dirty_read(settings, K) of
                          [{_,K,V}] -> V;
                          _ -> error("No such setting:" ++ K)
                      end
              end, Keys).


%% @spec get_settings() -> [{Key,Val}]
get_settings() ->
    T = fun () ->
                R = mnesia:select(settings, [{#settings{_ = '_'}, [],['$_']}]),
                lists:map(fun ({_,K,V}) -> {K,V} end, R)
        end,
    case mnesia:transaction(T) of
        {atomic, R} -> R;
        {aborted, Reason} -> error(Reason)
    end.

%% @spec load_ip_database() -> ok
load_ip_database() ->
    DBFile = eproxychk_util:basedir(["priv", ?GEO_DB_FILE]),
    read_csv(DBFile,
             fun () ->
                     ?LOG_INFO("loading geoip data into mnesia db"),
                     mnesia:delete_table(country),
                     mnesia:delete_table(geoip),
                     mnesia:create_table(country,
                                         [{attributes, record_info(fields, country)},
                                          {type, ordered_set}]),
                     mnesia:create_table(geoip,
                                         [{attributes, record_info(fields, geoip)},
                                          {type, ordered_set}])
             end,
             fun([IPFrom,IPTo, Reg, _,Cn,_Cnt,Cntry|_]) ->
                     %%io:format("going to write~n",[]),
                     mnesia:dirty_write(#geoip{%%inserted = now(),
                                               %% ip_from = list_to_integer(IPFrom),
                                               %% ip_to = list_to_integer(IPTo),
                                               ip_range = {list_to_integer(IPFrom),list_to_integer(IPTo)},
                                               registry = Reg,
                                               cn = Cn,
                                               country = Cntry}),
                     %% countries
                     case Cn /= "ZZ" of
                         true ->
                             mnesia:dirty_write(#country{cn = Cn, country = Cntry});
                         _ -> ok
                     end
             end,
             fun() ->
                     %%mnesia:add_table_index(geoip, ip_range),
                     %% mnesia:add_table_index(geoip, ip_from),
                     %% mnesia:add_table_index(geoip, ip_to),
                     mnesia:dump_tables([country, geoip]),
                     ?LOG_INFO("done loading!"),
                     ok
             end).
            

country_by_ip(Ip) ->
    Ip1 = eproxychk_util:ip_to_int(Ip),
    MatchHead =
        #geoip{ip_range={'$1','$2'}, cn='$3', _ = '_'},
    Guards = [{'=<', '$1', Ip1},{'>=', '$2', Ip1}],
    Results = ['$3'],
    case mnesia:dirty_select(geoip, [{MatchHead, Guards, Results}]) of
        [R|_] -> R;
        [] -> "unknown";
        E -> error(E)
    end.
    
%%io:format("~p~n",[ets:fun2ms(fun({M,N,Z}) when N >= 10; M =< 10 -> Z end)]),
%% country_by_ip(Ip) ->
%%     Ip1 = eproxychk_util:ip_to_int(Ip),
%%     F = fun() ->
%%                 Q = qlc:q([Cn || #geoip{ip_range={From,To}, cn=Cn} <- mnesia:table(geoip),
%%                                 From =< Ip1, To >= Ip1
%%                           ]),
%%                 qlc:e(Q)
%%         end,
%%     case mnesia:transaction(F) of
%%         {atomic, [R|_]} -> R;
%%         {atomic, []} -> "unknown";
%%         {aborted, Reason} ->
%%             ?LOG_ERR(Reason),
%%             "unknown"
%%     end.

get_countries() ->
    mnesia:dirty_match_object(#country{cn='_', country='_'}).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% load countries on first start
set_defaults() ->
    case mnesia:table_info(geoip,size) of
        0 -> load_ip_database();
        _ -> ok
    end.

