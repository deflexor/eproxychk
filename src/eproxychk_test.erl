-module(eproxychk_test).

-include("eproxychk.hrl").
-import(lists, [map/2]).
-import(proplists, [get_value/2]).
%% API
-export([stress_db_upd/1]).
-compile(export_all).
-define(DB, eproxychk_mongo).


%%#####################
%% API
%%#####################
stress_db_upd(L) ->
    db_start(),
    F = fun() ->
                do_upd_stress_test(L)
        end,
    timedo(F).

stress_check_types(L) ->
    F = fun() ->
                do_checktypes_stress_test(L)
        end,
    timedo(F).

stress_db_countrybyip(Mod, L) ->
    db_start(),
    F = fun() ->
                do_byip_stress_test(Mod, L)
        end,
    timedo(F).
    
stress_db_listcountry(Mod, L) ->
    db_start(),
    F = fun() ->
                Seq = lists:seq(1,L),
                lists:foreach(fun(_) ->
                                      Mod:get_countries()
                              end,Seq)
        end,
    timedo(F).
    

%%####################
%% Internal
%%####################
do_checktypes_stress_test(Limit) ->
    {Srv,_} = ?DB:get_servers([{"type", "unknown"}, {"enabled", false}]
                              ,Limit,0,
                              [{"updated", asc}]),
    P = map(fun(S) ->
                    F = fun() ->
                                _S1 = check_type(S)
                                %%io:format("updated ~p~n", [S1])
                        end,
                    {Pid, _} = spawn_opt(F, [monitor]),
                    Pid
            end, Srv),
    wait_for_pids(P).

check_type(S) ->
    Host = ?MAP_VALBB("host", S),
    Port = list_to_integer(?MAP_VALBB("port", S)),
    T = eproxychk_pxutil:check_type(Host,Port),
    io:format("check_type(~p) -> ~p~n",[Host, T]),
    T.
    %%?MAP_UPDB("type", atom_to_list(T), S).


do_upd_stress_test(Limit) ->
    Srv = ?DB:get_servers_for_update(Limit, 0, 0),
    P = map(fun(S) ->
                    F = fun() ->
                                S1 = proplists:delete(<<"_id">>, S),
                                _R = ?DB:update_server(S1)
                                %%io:format("updated ~p~n", [R])
                        end,
                    {Pid, _} = spawn_opt(F, [monitor]),
                    Pid
            end, Srv),
    wait_for_pids(P).

do_byip_stress_test(Mod,Limit) ->
    Srv = eproxychk_mongo:get_servers_for_update(Limit, 0, 0),
    P = map(fun(S) ->
                    F = fun() ->
                                H = ?MAP_VALBB("host", S),
                                R = Mod:country_by_ip(H),
                                %%io:format("country_by_ip(~p) -> ~p~n", [H,R])
                                R
                        end,
                    {Pid, _} = spawn_opt(F, [monitor]),
                    Pid
            end, Srv),
    wait_for_pids(P).

wait_for_pids([]) ->
    ok;
wait_for_pids(Pids) ->
    receive
        {'DOWN',_,process,Pid,normal} ->
            wait_for_pids(Pids -- [Pid]);
        {'DOWN',_,process,Pid,St} ->
            io:format("got error ~p (~p)~n",[Pid, St]),
            wait_for_pids(Pids -- [Pid]);
        Msg ->
            io:format("got unknown ~p~n",[Msg]),
            wait_for_pids(Pids)
    after 30000 ->
            io:format("looks like we hang here~n",[])
    end.

db_start() ->
    application:start(emongo),
    emongo:add_pool(mongo_pool, "localhost", 27017, "eproxychk", 50).

timedo(F) ->
    {Diff, Resp} = timer:tc(F),
    SpeedSec = lists:append(io_lib:format("~.2f", [Diff * 0.000001])),
    {list_to_float(SpeedSec), Resp}.
