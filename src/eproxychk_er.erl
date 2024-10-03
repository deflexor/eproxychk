%%%-------------------------------------------------------------------
%%% File    : eproxychk_er.erl
%%% Author  : dfr
%%% Description : 
%%%
%%% Created : 31 Dec 2011 by dfr
%%%-------------------------------------------------------------------
-module(eproxychk_er).
-behaviour(gen_server).
-include("eproxychk.hrl").
-import(lists, [foreach/2, map/2]).
%% API
-export([start_link/0, start/0, stop/0]).
-export([status/0, set_batchsize/1, s/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%%-compile(export_all).
-define(SERVER, ?MODULE).
-define(LOOP_TIME, 2000).
-define(MAX_TYPE_SCANS, 5).

-record(state, {
          pids = [],
          dboffset = 0,
          batch = 10,
          proc_err = [],
          myip
}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    ?MODULE:start_link().

stop() -> gen_server:call(?MODULE, stop).

status() -> gen_server:call(?MODULE, status).
s() -> gen_server:call(?MODULE, status1).
set_batchsize(Size) -> gen_server:cast(?MODULE, {set_batchsize,Size}).


%%====================================================================
%% gen_server
%%====================================================================
init([]) ->
    Batch = eproxychk_mongo:get_setting("checker_batch_size", 10),
    erlang:send_after(0, self(), trigger),
    %%erlang:send_after(500, self(), s),
    {ok, #state{batch = Batch, myip = eproxychk_util:my_ip()}}.


handle_cast({set_batchsize, NewSize}, State) ->
    case is_integer(NewSize) of
        true ->
            {noreply, State#state{batch=NewSize}, ?LOOP_TIME};
        _ ->
            {noreply, State}
    end;
handle_cast(Msg, State) ->
    io:format("Cast msg ~p~n", [Msg]),
    {noreply, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(status, _, #state{pids=Pids, batch=Batch, dboffset=Off, proc_err=Pe}=State) ->
    {Ctot, Cup} = eproxychk_mongo:servers_info(),
    %% sys:get_status(eproxychk_er).
    %%S = io_lib:format("pids:~p~nbatch:~p~noffset:~p~ntotal servers:~p~nfor update:~p~n",[length(Pids),Batch, Off, Ctot, Cup]),
    Stat = [{"npids",length(Pids)},
            {"batch", Batch},
            {"offset",Off},
            {"srv_total",Ctot},
            {"srv_update", Cup},
            {"status", <<"on">>},
            {"proc_err", Pe}],
    {reply, Stat, State#state{proc_err = []}, ?LOOP_TIME};
handle_call(Msg, From, State) ->
    io:format("unknown msg ~p from ~p ~n",[Msg,From]),
    {reply, ok, State}.

handle_info(trigger, #state{pids=Pids, batch=Batch, dboffset=Off, myip=MyIP}=State) ->
    MyBatch = Batch - length(Pids),
    %%io:format("pids running:~p, batch diff:~p, offset:~p~n",[length(Pids),MyBatch, Off]),
    {Ps,Noff} = case MyBatch > 0 of
                    true ->
                        Intl = list_to_integer(binary_to_list(eproxychk_mongo:get_setting("scan_interval", <<"604800">>))),
                        Servers = eproxychk_mongo:get_servers_for_update(MyBatch, Off, Intl),
                        %%io:format("servers: ~p ~p~n", [Intl, Servers]),
                        %% new offset for db select, reset to 0 if no more servers to
                        %% update left
                        NewOff = case length(Servers) of
                                     0 -> 0;
                                     L -> Off + L
                                 end,
                        %% spawn checker processes, 1 process for 1 server found
                        %% and return list of pids
                        {map(fun(S) ->
                                     F = fun() ->
                                                 S1 = proplists:delete(<<"_id">>, S),
                                                 %%io:format("done1~p~n",[S1]),
                                                 R = check_one_server(S1, MyIP),
                                                 %%io:format("done2~p~n",[R]),
                                                 eproxychk_mongo:update_server(R)
                                                 %%exit(normal)
                                         end,
                                     {Pid, _} = spawn_opt(F, [monitor]),
                                     Pid
                             end, Servers), NewOff};
                    _ ->
                        %%io:format("Queue is full~n", []),
                        {[], Off}
                end,
    erlang:send_after(?LOOP_TIME, self(), trigger),
    {noreply, State#state{pids = Pids ++ Ps, dboffset=Noff}};
handle_info({'DOWN', _Ref, _, Pid, Info}, #state{pids=Pids, proc_err=Pe}=State) ->
    _Pe = case Info of
              normal -> Pe;
              {Err,St} ->
                  error_logger:error_msg("Proc down ~p (~p~n~p)~n", [Pid, Err, St]),
                  %% keep last 10 errors
                  case catch element(1, Err) of
                      {'EXIT',_} -> Pe;
                      E -> lists:sublist([E | Pe], 10)
                  end
          end,
    Pids1 = lists:delete(Pid, Pids),
    {noreply, State#state{proc_err = Pe, pids = Pids1}};
handle_info(s,  #state{pids=Pids} = State) ->
    _Tat = map(fun(P) ->
                       {memory,M} = erlang:process_info(P, memory),
                       [{current_function,F}|_] = process_info(P),
                       io:format("~p: func: ~p ~p~n",[P, F, M]),
                       case M > 100000 of
                           true -> io:format("~p: PI: ~p~n",[P, erlang:process_info(P)]);
                           _ -> ok
                       end
               end, Pids),
    erlang:send_after(200, self(), s),
    {noreply, State};
handle_info(Msg, State) ->
    io:format("unknown info msg ~p ~n",[Msg]),
    {reply, ok, State}.

terminate(_Reason, #state{pids=Pids}) ->
    foreach(fun(Pid) -> exit(Pid, noproc) end, Pids),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%====================================================================
%% Internal functions
%%====================================================================
%% @spec check_one_server(Server) -> Server
%% @spec Server = [{Key, Val}]
%% @spec Key = binary()
%% @spec Val = binary() | string() | integer() | float()
%% @doc Check single proxy for various metrics
check_one_server(S, Ip) ->
    %%io:format("cheking server: ~s:~s~n", [?MAP_VALBB("host", S), ?MAP_VALBB("port", S)]),
    S1 = check_country(S),
    S2 = check_type(S1),
    S3 = check_speed(S2, Ip),
    S4 = ?MAP_UPDB("scans", ?MAP_VALB("scans", S, 0) + 1, S3),
    S5 = check_stability(S4),
    %%io:format("checked! ~s:~s ~p~n", [?MAP_VALBB("host", S), ?MAP_VALBB("port", S), S4]),
    S5.

check_stability(S) ->
    Stb = case ?MAP_VALB("scans", S, 0) of
              Scans when Scans > 0 ->
                  Ok = ?MAP_VALB("ok_scans", S, 0),
                  trunc(Ok / Scans * 100);
              _ -> 0
          end,
    ?MAP_UPDB("stability", Stb, S).

%% @spec check_speed(Server) -> Server
%% @doc Check proxy for responsiveness
%% and optionally for transparency
check_speed(S, MyIp) ->
    [Url,Pchat, Ip] = map(fun erlang:binary_to_list/1, eproxychk_mongo:get_settings(["checker_url", "checker_chat","my_ip"])),
    MyIp1 = case Ip of [] -> MyIp; I -> I end,
    Host = ?MAP_VALBB("host", S),
    Port = list_to_integer(?MAP_VALBB("port", S)),
    case ?MAP_VALBB("type", S) of
        Ptype when Ptype =/= "unknown", Ptype =/= undefined ->
            case eproxychk_pxutil:check_speed(Host, Port, Ptype, {Url,Pchat, MyIp1}) of
                {ok, Speed, Anon} ->
                    S1 = ?MAP_UPDB("speed", Speed, S),
                    S2 = ?MAP_UPDB("anon", Anon, S1),
                    ?MAP_UPDB("ok_scans", ?MAP_VALB("ok_scans", S, 0) + 1, S2);
                {error, Err} ->
                    S1 = ?MAP_UPDB("speed", 9999, S),
                    S2 = ?MAP_UPDB("error", io_lib:format("~p",[Err]), S1),
                    ?MAP_UPDB("ok_scans", ?MAP_VALB("ok_scans", S, 0), S2)
            end;
        _ -> ?MAP_UPDB("ok_scans", ?MAP_VALB("ok_scans", S, 0), S)
    end.

%% @spec check_country(Server) -> Server
%% @doc Find proxy country by host
check_country(S) ->
    Ip = ?MAP_VALBB("host", S),
    case ?MAP_VALB("country", S) of
        undefined -> ?MAP_UPDB("country", eproxychk_mnesia:country_by_ip(Ip), S);
        _ -> S
    end.

%% @spec check_type(Server) -> Server
%% @doc Find proxy type by host
check_type(S) ->
    Host = ?MAP_VALBB("host", S),
    Port = list_to_integer(?MAP_VALBB("port", S)),
    Scans = ?MAP_VALB("scans", S, 0),
    Stab = ?MAP_VALB("stability", S, 0),
    case ?MAP_VAL("type", S) of
        T when T =:= "unchecked"; T =:= "unknown"; T =:= undefined ->
            case eproxychk_pxutil:check_type(Host,Port) of
                unknown when Scans > ?MAX_TYPE_SCANS, Stab =:= 0 ->
                    %% exclude from checking after N unsuccesful scans
                    ?MAP_UPDB("enabled", false, S);
                Type ->
                    ?MAP_UPDB("type", atom_to_binary(Type, latin1), S)
            end;
        _ -> S
    end.


