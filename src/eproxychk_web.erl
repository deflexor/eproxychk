%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for eproxychk.

-module(eproxychk_web).
-author("Mochi Media <dev@mochimedia.com>").
-include("eproxychk.hrl").
-import(lists, [map/2, filter/2, keystore/4, keysort/2, ukeymerge/3]).

-export([start/1, stop/0, loop/2]).

-define(RANGE_STR(Offset,Limit,Total), "items " ++ integer_to_list(Offset) ++ "-" ++ integer_to_list(Offset+Limit) ++ "/" ++ integer_to_list(Total)).
-define(FIELDS, ["host","port","type","anon","country","speed","stability","scans","ok_scans"]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    %% How we validate origin for cross-domain checks:
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "proxylist" ++ _ -> c_proxylist(Req);
                    "countrylist" ++ _ -> c_countrylist(Req);
                    "headers" -> c_yourheaders(Req);
                    "myip" -> c_myip(Req);
                    "svcinfo" -> c_svc_info(Req);
                    "checker/start" -> c_svc_start(eproxychk_er, Req);
                    "checker/stop" -> c_svc_stop(eproxychk_er, Req);
                    "parser/start" -> c_svc_start(eproxychk_parser, Req);
                    "parser/stop" -> c_svc_stop(eproxychk_parser, Req);
                    "setting/get" -> c_setting_get(Req);
                    F ->
                        %%H = mochiweb_headers:from_binary([<<"Content-Type: text/html; charset=UTF-8\r\n">>]),
                        Req:serve_file(F, DocRoot)
                end;
            'POST' ->
                case Path of
                    "checker/setbatch" -> c_checker_setbatch(Req);
                    "setting/savemany" -> c_save_settings(Req);
                    "server/setbyhostport" -> c_server_update(Req);
                    %%"comet" -> wsloop_active();
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %%io:format("web error: path:~p ~p:~p~n",[Path, What,erlang:get_stacktrace()]),
            %%?LOG_ERR("web:" ++  Type ++ What),
            %% NOTE: mustache templates need \\ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\\n"})
    end.

%% controllers
c_setting_get(Req) ->
    {_, Qs, _} = mochiweb_util:urlsplit_path(Req:get(raw_path)),
    {struct,[{<<"key">>,Key}]} = mochijson2:decode(mochiweb_util:unquote(Qs)),
    Val = eproxychk_mongo:get_settings([Key]),
    %%io:format("~p~n", [Val]),
    render_json(Req, Val, []).

c_myip(Req) ->
    Ip = eproxychk_util:my_ip(),
    %%io:format("~p~n", [Val]),
    render_json(Req, list_to_binary(Ip), []).

c_yourheaders(Req) ->
    Ip = Req:get(peer),
    Hdrs1 = mochiweb_headers:to_list(Req:get(headers)),
    F = fun ({K,V}, Acc) ->
                Acc ++ io_lib:format("~p: ~s~n",[K,V]) end,
    Hdrs = lists:foldl(F, "", Hdrs1),
    Req:ok({"text/plain", "IP:" ++ Ip ++ "\n" ++ Hdrs}).

c_countrylist(Req) ->
    CountryData = eproxychk_mnesia:get_countries(),
    Data1 = map(fun (#country{cn=Cn,country=Country}) ->
                        [{cn, list_to_binary(Cn)},
                         {country, list_to_binary(Country)}]
                end, CountryData),
    Data2 = [{identifier, <<"cn">>}, {label, <<"country">>}, {items, Data1}],
    render_json(Req, Data2, []).
    

c_proxylist(Req) ->
    Qs = Req:parse_qs(),
    {Offset,Limit} = proxylist_offset_limit(Req:get(headers)),
    %%io:format("limit:~p, offset:~p~n", [Limit, Offset]),
    Query = proxylist_query(Qs),
    Sort = proxylist_sort(Qs),
    {Data,Total} = eproxychk_mongo:get_servers(Query, Limit+1, Offset, [Sort]),
    Data1 = map(fun ([_Id|R]) -> R end, Data),
    %%io:format("~p~n~p~n~n", [Query, Data1]),
    render_json(Req, Data1, [{"Content-Range", ?RANGE_STR(Offset, Limit, Total)}]).

c_svc_start(Mod, Req) ->
    S = case catch apply(Mod,start,[]) of
            {ok, _Pid} -> ok;
            {error, {already_started,_}} -> ok;
            Err ->
                ?LOG_ERR(Err),
                error
    end,
    render_json(Req, S, []).

c_svc_stop(Mod, Req) ->
    catch apply(Mod,stop,[]),
    render_json(Req, ok, []).

c_svc_info(Req) ->
    F = fun(Mod) ->
                case (catch apply(Mod,status,[])) of
                    {'EXIT',{R,_}} -> [{"status", R}];
                    St -> St
                end
        end,
    S = [{"checker", F(eproxychk_er)}, {"parser", F(eproxychk_parser)}],
    render_json(Req, S, []).

c_checker_setbatch(Req) ->
    [Size] = mochijson2:decode(Req:recv_body()),
    R = case catch list_to_integer(binary_to_list(Size)) of
            S when is_integer(S) ->
                eproxychk_mongo:set_setting("checker_batch_size", S),
                eproxychk_er:set_batchsize(S),
                ok;
            {'EXIT',{Err,_}} -> [{ error, Err }]
        end,
    render_json(Req, R, []).

c_save_settings(Req) ->
    {struct,[{<<"obj">>,{struct,Params}}]} = mochijson2:decode(Req:recv_body()),
    %%io:format("~p~n", [Params]),
    map(fun({K,V}) -> eproxychk_mongo:set_setting(K, V) end, Params),
    render_json(Req, ok, []).

c_server_update(Req) ->
    {struct,R} = mochijson2:decode(Req:recv_body()),
    Host = ?MAP_VALB("host", R),
    Port = ?MAP_VALB("port", R),
    {struct, Obj} = ?MAP_VALB("obj", R),
    %%Obj = map(fun({K,V}) -> case V of true -> {K,1}; false -> {K,0} end end, Obj1),
    FindDoc = [{<<"host">>, Host},{<<"port">>, Port}],
    case eproxychk_mongo:get_servers(FindDoc, 1, 0) of
        {[D],_} ->
            UpDoc1 = ukeymerge(1, FindDoc, keysort(1, Obj)),
            UpDoc = ukeymerge(1, UpDoc1, keysort(1,D)),
            %%io:format("~p:~n", [UpDoc]),
            eproxychk_mongo:update_server(UpDoc);
        _ ->
            ok
    end,
    render_json(Req, ok, []).


%% Internal API
proxylist_query(Qs) ->
    Fs = [%% common filter
          fun(Q) ->
                  filter(fun({K,V}) when is_list(V), length(V) > 0 ->
                                 lists:member(K, ?FIELDS);
                            (_) -> false
                         end, Q)
          end,
          %% okonly
          fun(Q) ->
                  case proplists:get_value("okonly",Qs) of
                      undefined -> Q;
                      _ -> keystore("ok_scans", 1, Q, {"ok_scans", [{'>', 0}]})
                  end
          end,
          %% types
          fun(Q) ->
                  Types = proplists:get_all_values("types", Qs),
                  keystore("types", 1, Q, {"type", [{in, Types}]})
          end,
          %% stability
          fun(Q) ->
                  K = "stability",
                  V = list_to_integer(proplists:get_value(K,Q,"0")),
                  lists:keyreplace(K, 1, Q, {K, [{gte, V}]})
          end,
          %% speed
          fun(Q) ->
                  K = "speed",
                  V = list_to_integer(proplists:get_value(K,Q,"9999")),
                  lists:keyreplace(K, 1, Q, {K, [{lte, V}]})
          end],
    %%io:format("> ~p~n", [Qs]),
    Qr = eproxychk_util:fseq(Fs, Qs),
    %%io:format("< ~p~n", [Qr]),
    Qr.

proxylist_offset_limit(H) ->
    %%io:format("~p~n", [mochiweb_headers:get_value("Range", H)]),
    case mochiweb_headers:get_value("Range", H) of
                         "items=" ++ R ->
                             [From, To] = map(fun list_to_integer/1,
                                              string:tokens(R, "-")),
                             {From, To-From};
                         _ -> {0, 24}
    end.

proxylist_sort(Qs) ->
    case proplists:get_value("sort",Qs) of
        [$\s|Fld] -> {Fld, asc};
        [$-|Fld] -> {Fld, desc};
        _ -> {"host", 1}
    end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

render_json(Req, Data, Hdrs) ->
    Output = mochijson2:encode(Data),
    Req:ok({"text/plain; charset=utf-8", Hdrs, Output}).
%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.


