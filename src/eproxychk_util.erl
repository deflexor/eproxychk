-module(eproxychk_util).
-include("eproxychk.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(lists, [map/2]).
-export([epoch/0, basedir/1, read_csv/4, ip_to_int/1, my_ip/0, with_tcp_sock/4, fseq/2]).

-define(TCP_OPTIONS, [inet, binary,
                      {packet, 0},
                      {active, false},
                      {nodelay, true},
                      {reuseaddr, true}]).


%% @spec epoch() -> integer()
epoch() ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600.

%% @spec basedir([PathPart]) -> string()
basedir(Path) ->
    filename:join([basedir() | Path]).

%% @spec basedir() -> string()
basedir() ->
    {file, Path} = code:is_loaded(?MODULE),
    filename:dirname(filename:dirname(Path)).

%% @spec read_csv(Filename, Exists_Func, ReadLine_Func) -> ok | {error, Reason}
read_csv(Filename, FExists, FReadline, FFin) ->
    case file:open(Filename,[read]) of
        {ok, FH } ->
            try
                FExists(),
                read_csv_lines(FH, FReadline)
            after
                file:close(FH),
                FFin()
            end;
        E -> E
    end.
    
read_csv_lines(FH, FReadline) ->
    case file:read_line(FH) of
        {ok, Data} ->
            case Data of
                "#" ++ _ -> ok; % skip comments
                D ->
                    D1 = string:strip(D, right, $\n),
                    Cols = string:tokens(D1,",;"),
                    FReadline(map(fun(C) -> string:strip(C, both, $") end, Cols))
            end,
            read_csv_lines(FH, FReadline);
        {error, R } ->
            io:format("Error reading file: ~p~n", [R]);
        _ ->
            ok
    end.

ip_to_int({A,B,C,D}) ->
    (A*16777216)+(B*65536)+(C*256)+(D);
ip_to_int(IpStr) ->
    case inet_parse:address(IpStr) of
        {ok, Ip} -> ip_to_int(Ip);
        E -> E
    end.

my_ip() ->
    F = fun(S) ->
                case inet:sockname(S) of
                    {ok, {Ip, _Port}} ->
                        inet_parse:ntoa(Ip);
                    _ -> ""
                end
        end,
    try
        with_tcp_sock(?GOOGLE_IP, 53, F, 5000)
    catch
        error:_ -> ""
    end.
    
with_tcp_sock(Ip, Port, F, Timeout) ->
    case gen_tcp:connect(Ip, Port, ?TCP_OPTIONS, Timeout) of
        {ok, S} ->
            try
                F(S)
            after
                gen_tcp:close(S)
            end;
        E -> E
        %% {error, Err} ->
        %%     %%error_logger:error_msg("error connecting to ~s:~p - ~p~n", [Ip,Port, Err]),
        %%     error(Err)
    end.

%% tests
ip_to_int_test() ->
    16909060 = ip_to_int("1.2.3.4"),
    16843009 = ip_to_int("1.1.1.1"),
    4294967295 = ip_to_int("255.255.255.255").

%% run in sequence list of functions, feeding result of previous function to next one
fseq([F|Fs], Param) ->
    fseq(Fs, F(Param));
fseq([], Param) -> Param.
    
