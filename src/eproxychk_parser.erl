%%%-------------------------------------------------------------------
%%% File    : eproxychk_parser.erl
%%% Author  : dfr
%%% Description : http client, делает запрос к прокси и возвращает
%%%               оценки
%%%
%%% Created : 29 Dec 2011 by dfr
%%%-------------------------------------------------------------------
-module(eproxychk_parser).
-behaviour(gen_server).
-include("eproxychk.hrl").

%% gen_server callbacks
-export([start_link/0, start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% API
-export([pfire_find_topic_urls/1, status/0]).
-export([pfire_find_servers/1]).
-import(lists, [map/2, append/1, foldl/3]).
-compile(export_all).

-define(LOOP_TIME, 1000 * (1 + random:uniform(4))).
-define(SERVER, ?MODULE).
-define(DELAY(F), fun() -> F end).
-define(FORCE(F), F()).
-define(SEEK_PAGES, 1).
-define(PFIRE_URL, "http://www.proxyfire.net/forum/").

-record(state, {
          page_urls = [],
          topic_urls = [],
          saved = 0,
          found = 0
}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    ?MODULE:start_link().

stop() -> gen_server:call(?MODULE, stop).

status() ->
    gen_server:call(?MODULE, status, 100).

%%====================================================================
%% gen_server
%%====================================================================
init([]) ->
    ibrowse:start(),
    PageUrls = [ "forumdisplay.php?f=" ++ integer_to_list(Fid)
                 ++ "&order=desc&page=" ++ integer_to_list(Page)
                 || Fid <- [14,15,16], Page <- lists:seq(1,?SEEK_PAGES) ],
    erlang:send_after(0, self(), trigger),
    {ok, #state{page_urls=PageUrls}}.


handle_cast(Msg, State) ->
    io:format("Cast msg ~p~n", [Msg]),
    {noreply, State}.

handle_call(status, _From, #state{topic_urls=Tu,page_urls=Pu,saved=S,found=P}=State) ->
    Stat = [
            {"status", <<"on">>},
            {"saved", S},
            {"found", P},
            {"ntopics", length(Tu)},
            {"npages", length(Pu)}
           ],
    %%io:format("~p~n",[State]),
    {reply, Stat, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Msg, From, State) ->
    io:format("unknown msg ~p from ~p ~n",[Msg,From]),
    {reply, ok, State}.

handle_info({trigger_saved, S1, P1}, #state{saved=S,found=P}=State) ->
    erlang:send_after(?LOOP_TIME, self(), trigger),
    {noreply, State#state{saved=S+S1,found=P+P1}};
handle_info({trigger_new_urls, TopicUrls1}, #state{topic_urls=TopicUrls}=State) ->
    erlang:send_after(?LOOP_TIME, self(), trigger),
    {noreply, State#state{topic_urls=TopicUrls1 ++ TopicUrls}};
handle_info(trigger, #state{topic_urls=[Tu|TopicUrls]}=State) ->
    GSPid = self(),
    F = fun() ->
                Servers = pfire_find_servers(Tu),
                %%io:format("saving servers from ~p (~p)~n",[Tu, length(Servers)]),
                S = save_servers(Servers),
                P = length(Servers),
                GSPid ! {trigger_saved, S, P }
        end,
    spawn_opt(F, [monitor, link]),
    {noreply, State#state{topic_urls=TopicUrls}};
handle_info(trigger, #state{page_urls=[Pu|PageUrls], topic_urls=[]}=State) ->
    GSPid = self(),
    F = fun() ->
                TopicUrls = pfire_find_topic_urls(?PFIRE_URL ++ Pu),
                GSPid ! {trigger_new_urls, TopicUrls }
        end,
    spawn_opt(F, [monitor, link]),
    {noreply, State#state{page_urls=PageUrls}};
handle_info(trigger, State) ->
    erlang:send_after(?LOOP_TIME, self(), trigger),
    {noreply, State};
handle_info({'DOWN', _Ref, _, _Pid, _Info}, State) ->
    {noreply, State};
handle_info(Msg, State) ->
    io:format("info unknown msg ~p ~n",[Msg]),
    {noreply, State}.

terminate(_Reason, #state{}=_S) ->
    ibrowse:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% API
%%====================================================================
    

%%====================================================================
%% Internal functions
%%====================================================================
%% @spec save_servers([string()]) -> integer()
save_servers(Servers) ->
    foldl(fun(S, Cnt) ->
                  [ Host, Port ] = string:tokens(S," :"),
                  
                  %% Cnt + case Cnt < 10 of
                  %%           true ->eproxychk_mongo:insert_server(Host, Port);
                  %%           _ -> 0
                  %%       end
                                       
                  Cnt + eproxychk_mongo:insert_server(Host, Port)
                  end, 0, Servers).

pfire_find_topic_urls(Url) ->
    Body = fetch_as_string(Url),
    Tree = mochiweb_html:parse(Body),
    Links = mochiweb_xpath:execute("//a[substring(@id,1,6) = 'thread']",Tree),
    Hrefs = map(fun ({_, Attrs,_}) ->
                        Href = proplists:get_value(<<"href">>, Attrs, <<"">>),
                        ?PFIRE_URL ++ binary_to_list(Href)
                end, Links),
    lists:filter(fun(X) -> X /= ?PFIRE_URL end, Hrefs).
                    
    
pfire_find_servers(TopicUrl) ->
    try
        Body = fetch_as_string(TopicUrl),
        Pat = "(\\d{1,3}\.\\d{1,3}\.\\d{1,3}\.\\d{1,3}:\\d{1,5})",
        case re:run(Body, Pat, [global,{capture,[1],list}]) of
            {match, IPLists} -> append(IPLists);
            _ -> []
        end
    catch
        error:E ->
            ?LOG_ERR("pfire_find_servers error: " ++ E),
            []
    end.
    

fetch_as_string(Url) ->
    Headers = [{"User-Agent", ?UA}],
    Resp = ibrowse:send_req(Url, Headers, get, [], [], 9000),
    case Resp of
        {ok,_Status,_Hdrs,Body} ->
            Body;
        {error,Reason} ->
            ?LOG_ERR("Error fetching URL:'" ++ Url ++ "': " ++ Reason),
            []
    end.
