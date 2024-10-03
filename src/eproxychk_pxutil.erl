%%%-------------------------------------------------------------------
%%% File    : eproxychk_client.erl
%%% Author  : dfr
%%% Description : http client, делает запрос к прокси и возвращает
%%%               оценки
%%%
%%% Created : 29 Dec 2011 by dfr
%%%-------------------------------------------------------------------
-module(eproxychk_pxutil).
-include_lib("kernel/include/inet.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("eproxychk.hrl").
-import(lists, [foldl/3]).

%% API
%%-export([check_type/2, check_speed/4]).
-compile(export_all).
-define(CHK_TIMEOUT, 5000).
-define(HTTP_TIMEOUT, 180000).
-define(BUF, 16384).
-define(MAX_BODY_LEN, 16384).


%%====================================================================
%% API
%%====================================================================

check_type(Ip, Port) ->
    %% check for HTTP
    Fhttp = type_checker_fun(http_test_req(),
                        fun ({ok, <<"HTTP", _/binary>>}) -> true;
                            (_) -> false
                        end),
    FSock4 = type_checker_fun(socks4_hello_req(?GOOGLE_IP, 53),
                         fun ({ok, <<0, 5:4, _:4, _:48>>}) -> true;
                             (_) -> false
                         end),
    FSock5 = type_checker_fun(socks5_hello_req(),
                         fun ({ok, <<16#5, 16#ff>>}) -> true;
                             ({ok, <<16#5, 0>>}) -> true;
                             (_) -> false
                         end),
    try
        foldl(fun ({Proto, F},unknown=A) ->
                      case eproxychk_util:with_tcp_sock(Ip, Port, F, ?CHK_TIMEOUT) of
                          true -> Proto;
                          _ -> A
                      end;
                  (_,A) -> A
              end, unknown, [{socks5, FSock5}, {socks4, FSock4}, {http, Fhttp}])
    catch
        error:_ -> err
    end.

%% eproxychk_pxutil:check_speed("94.26.146.7", 8080, "http").
%% eproxychk_pxutil:check_speed("208.189.20.32", 3128, "http").
%% eproxychk_pxutil:check_speed("195.254.186.7", 9050, "socks5").
%% eproxychk_pxutil:check_speed("109.69.4.239", 8741, "socks4").
%% eproxychk_pxutil:check_speed("183.91.74.68", 8080, "http", {"http://murl.mobi/headers.php","murl.mobi", "208.43.125.100"}). ??
%% HTTP: "109.104.88.180",6515
check_speed(Host, Port, Ptype, {Url,Pchat, MyIp}) ->
    {ok, MyHost} = inet:gethostname(),
    F = fun(S) -> do_request(S, Url, Ptype) end,
    {Diff, Resp} = timer:tc(fun() -> eproxychk_util:with_tcp_sock(
                                       Host, Port, F, ?CHK_TIMEOUT) end),
    case Resp of
        {ok,Body} ->
            %%error_logger:error_msg("chp resp (~p) ~s~n", [Pchat, Body]),
            %%case string:str(Body, Pchat) of
            case re:run(Body, Pchat) of
                nomatch ->
                    {error, "chat string not found"};
                _ ->
                    SpeedSec = lists:append(io_lib:format("~.2f", [Diff * 0.000001])),
                    Trnsp = lists:any(fun (Pat) ->
                                                %io:format("~p~n", Body),
                                              re:run(Body, Pat,[]) /= nomatch
                                              %%string:str(Body, Pat) > 0
                                      end, [MyIp, MyHost]),
                    Anon = case Trnsp of true -> "transparent"; _ -> "anonymous" end,
                    {ok, list_to_float(SpeedSec), Anon }
            end;
        {error,Reason} ->
            {error,Reason}
    end.

%% fetch_url_as_str("http://" ++ _ = Url) ->
%%     {_,UrlHost,_,_,_} = mochiweb_util:urlsplit(Url),
%%     Host = url_to_ip(Url),
%%     Port = case string:tokens(UrlHost, ":") of
%%                [_, P] -> list_to_integer(P);
%%                _ -> 80
%%            end,
%%     F = fun(S) -> do_request(S, Url, "http") end,
%%     Resp = eproxychk_util:with_tcp_sock(
%%                      Host, Port, F, ?CHK_TIMEOUT),
%%     case Resp of
%%         {ok, Body} ->
%%             Body;
%%         {error, _E} -> ""
%%     end.


%%====================================================================
%% Internal functions
%%====================================================================
type_checker_fun(Rq, MFun) ->
    fun(S) ->
            case gen_tcp:send(S, Rq) of
                ok ->
                    MFun( gen_tcp:recv(S, 0, ?CHK_TIMEOUT ) );
                {error, Err} ->
                    error_logger:error_msg("tcp:send error ~p~n", [Err]),
                    false
            end
    end.


url_to_ip(Url) ->
    {_,UrlHost,_,_,_} = mochiweb_util:urlsplit(Url),
    [Host|_] = string:tokens(UrlHost,":"),
    case inet:gethostbyname(Host, inet) of
        {ok, #hostent{ h_addr_list =  [A|_] }} ->
            A;
        {error, _Err}=E -> E
    end.
            

http_test_req() ->
    <<"GET http://goo.gl/ HTTP/1.1\r\nHost: goo.gl\r\n\r\n">>.

socks4_hello_req(IpStr, Port) ->
    Ip = eproxychk_util:ip_to_int(IpStr),
    <<16#4, 16#1, Port:16, Ip:32, "hi", 0>>.

socks5_conn_req(Host, Port) ->
    L = string:len(Host),
    [<<16#5, 16#1, 0, 16#3, L:8>>, Host, <<Port:16>>].

socks5_hello_req() ->
    <<16#5, 16#1, 0>>.

recvResponse() ->
    receive
        {http, Socket, HttpPacket} ->
            case HttpPacket of
                {http_response, Version, Code, String} ->
                    inet:setopts(Socket,[{active, once}, {packet, http}]),
                    {ok, Version, Code, String};
                {http_error,Err} ->
                    {error, Err}
            end;
        {M, _} ->
            {error, M};
        U -> io:format("unkn resp msg: ~p~n", [U])
    after ?HTTP_TIMEOUT ->
            {error, timeout}
    end.

recvHdr(L) ->
    receive
        {http, Socket, HttpPacket} ->
            case HttpPacket of
                {http_response, Version, Code, String} ->
                    inet:setopts(Socket,[{active, once}, {packet, http}]),
                    R = {Version, Code, String},
                    recvHdr([R|L]);
                {http_header, _, Header, _, Value} ->
                    inet:setopts(Socket,[{active, once}, {packet, http}]),
                    H = {Header, Value},
                    recvHdr([H|L]);
                http_eoh ->
                    inet:setopts(Socket,[{active, once}, {packet, raw}]),
                    lists:reverse(L)
            end;
        {M, _} ->
            {error, M};
        U -> io:format("unkn hdr msg: ~p~n", [U])
    after ?HTTP_TIMEOUT ->
            ?LOG_ERR("recvHdr timeout"),
            []
    end.

%% erlang:process_info(self(), memory).
recvBody(Resp,CLen, Recvd) when Recvd >= CLen ->
    iolist_to_binary(lists:reverse(Resp));
recvBody(Resp,CLen, Recvd) ->
    receive
        {tcp, S, Chunk} ->
            %%io:format("~p: got chunk ~p bytes~n",[self(), byte_size(Chunk)]),
            ok = inet:setopts(S,[{active,once}]),
            recvBody([Chunk, Resp], CLen, Recvd + byte_size(Chunk));
            %%recvBody(<<Resp/binary, Chunk/binary>>, Left - byte_size(Chunk));
        {tcp_closed, _} ->
            %%io:format("got tcp_closed~p~n",[Resp]),
            iolist_to_binary(lists:reverse(Resp));
        {tcp_error, _, Reason} ->
            %%io:format("got tcp_error ~p~n",[Reason]),
            error_logger:error_msg("Error on read_response1, reason: ~p~n", [Reason]);
        U -> io:format("unkn body msg: ~p~n", [U])
    after ?HTTP_TIMEOUT ->
            ?LOG_ERR("recvBody timeout"),
            <<"">>
    end.        

do_request(S, "http://" ++ _ =Url, "http") ->
    {Sc,UrlHost,Path1,_,_} = mochiweb_util:urlsplit(Url),
    Path = case Path1 of [] -> "/"; P -> P end,
    %%Req = io_lib:format("GET ~s://~s~s HTTP/1.1\r\nHost: ~s\r\nAccept: */*\r\n\r\n", [Sc,UrlHost, Path, UrlHost]),
    Req = "GET " ++ Sc ++ "://" ++ UrlHost ++ Path ++ " HTTP/1.1\r\nHost: " ++ UrlHost ++ "\r\nAccept: */*\r\n\r\n",
    %%io:format("~p: doing req http4 ~p~n",[self(), Req]),
    case gen_tcp:send(S, list_to_binary(Req)) of
        ok ->
            ok = inet:setopts(S, [{packet, http}]),
            ok = inet:setopts(S,[{active,once}]),
            case recvResponse() of
                {ok,_,_,_} ->
                    gen_tcp:shutdown(S, write),
                    Hdr = recvHdr([]),
                    CLen = case list_to_integer(proplists:get_value('Content-Length', Hdr, "1")) of
                               L when L < ?MAX_BODY_LEN -> L;
                               _ -> ?MAX_BODY_LEN
                           end,
                    %%io:format("~p: rcv body with clen ~p~n",[self(), CLen]),
                    Body = recvBody([],CLen,0),
                    %%io:format("~p: body rcvd ~p bytes ~n",[self(), byte_size(Body)]),
                    {ok, Body};
                E ->
                    E
            end;
        {error, Err}=E ->
            error_logger:error_msg("tcp:send error ~p~n", [Err]),
            E
    end;
do_request(S, "http://" ++ _ =Url, "socks4") ->
    io:format("~p: doing req s4~n",[self()]),
    UrlIp = url_to_ip(Url),
    case gen_tcp:send(S, socks4_hello_req(UrlIp, 80)) of
        ok ->
            case gen_tcp:recv(S, 0, ?CHK_TIMEOUT ) of
                {ok, <<0, 5:4, 10:4, _:48>>} ->
                    do_request(S, Url, "http");
                E ->
                    error_logger:error_msg("socks4 bad response: ~p~n", [E]),
                    E
            end;
        {error, Err}=E ->
            error_logger:error_msg("tcp:send error ~p~n", [Err]),
            E
    end;
    
do_request(S, "http://" ++ _ =Url, "socks5") ->
    io:format("~p: doing req s5~n",[self()]),
    try
        {_,UrlHost,_,_,_} = mochiweb_util:urlsplit(Url),
        ok = gen_tcp:send(S, socks5_hello_req()),
        {ok, <<5, 0>>} = gen_tcp:recv(S, 0, ?CHK_TIMEOUT ),
        Rq2 = socks5_conn_req(UrlHost,80),
        ok = gen_tcp:send(S, Rq2),
        {ok, <<5, 0, 0, _Rest/binary>>} = gen_tcp:recv(S, 0, ?CHK_TIMEOUT ),
        do_request(S, Url, "http")
    catch
        error:_E ->
            %%error_logger:error_msg("socks5 chat error: ~p~n", [E]),
            {error, socks5}
    end.
%% do_request(S, Url, Proto) ->
%%     io:format("~p: doing req url:~p ~p~n",[self(), Url, Proto]),
%%     {error, badurl}.
%%do_request(S, "http://" ++ Url, Proto).

%%====================================================================
%% Tests
%%====================================================================
check_type_test() ->
    http = eproxychk_pxutil:check_type("77.88.21.3", 80).

check_speed1_test() ->
    Ch = {"http://www.ya.ru","yandex","192.168.1.22"},
    {ok,_Spd,"anonymous"} = eproxychk_pxutil:check_speed("77.88.21.3", 80, "http", Ch).
    

check_speed2_test() ->
    Ch = {"http://internet.yandex.ru/","myip","192.168.1.22"},
    {ok,_Spd,"anonymous"} = eproxychk_pxutil:check_speed("195.161.112.6", 80, "http", Ch).

do_request1_test1() ->
    {eok,_Resp} = eproxychk_util:with_tcp_sock("77.88.21.3", 80, fun(S) -> eproxychk_pxutil:do_request(S, "http://ya.ru", "http") end, 2000).

do_request2_test() ->
    {ok,_Resp} = eproxychk_util:with_tcp_sock("195.161.112.6", 80, fun(S) -> eproxychk_pxutil:do_request(S, "http://www.myip.ru/get_ip.php", "http") end, 2000).

