%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-29
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_web_client
%%% =================================================================

-module(sim_agent_httpc).
-author("Liu HuiDong").

-behaviour(gen_server).
-include("sim_agent.hrl").

-compile([export_all]).
%%% =================================================================
%%% API functions
%%% =================================================================
-export([start_link/0]).
-export([get_online_agent/1]).

%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%% -----------------------------------------------------------------
%%% Starts the server
%%% -----------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% -----------------------------------------------------------------
%%% @spec get_online_agent/1
%%% @return  {Online, Total} | HttpReturnBody
%%% -----------------------------------------------------------------
get_online_agent(Company) ->
    gen_server:call(?MODULE, {get_online_agent, Company}, 1000).

%%% -----------------------------------------------------------------
%%% @spec chk_qt/1
%%% @return  {Online, Total} | HttpReturnBody
%%% -----------------------------------------------------------------
chk_qt(ComId)->
   gen_server:call(?MODULE, {chk_qt, ComId}).

job(Type, ComId, Agents)->
    Data = #{<<"type">> => Type,
             <<"comid">> => ComId,
             <<"agents">> => Agents},
    sync_tweb("rexjob", "set", Data).

sync_tweb(Action, Cmd, Data)->
    gen_server:call(?MODULE, {post, Action, Cmd, Data}).

async_tweb(Action, Cmd, Data)->
    gen_server:cast(?MODULE, {post, Action, Cmd, Data}).
%%% =================================================================
%%% gen_server callbacks
%%% =================================================================

%%% -----------------------------------------------------------------
%%% init/1
%%% -----------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%% -----------------------------------------------------------------
%%% handle_call/3
%%% -----------------------------------------------------------------
handle_call({post, Action, Cmd, Data}, _From, State)->
    Act = filename:join(["/api/v1/", Action]),
    Reply = post(Act, Cmd, Data),
    {reply, Reply, State};

handle_call({chk_qt, ComId}, _From, State)->
   Action = "/api/v1/rexjob",
    Data = #{<<"type">> => 15, <<"comid">> => ComId, <<"agents">> => []},
    Reply = post(Action, "set", Data),
    {reply, Reply, State};

handle_call({get_online_agent, ComId}, _From, State = #state{}) ->
    Action = "/api/v1/agent",
    Data = #{<<"action">> => <<"counter">>,
             <<"comid">> => ComId},
    case post(Action, "get", Data) of
        #{<<"result">> :=[],
          <<"status">> := true} ->
            {reply, {0, 0}, State};
        #{<<"result">> :=
          [#{<<"active">> := Online,
             <<"total">> := Total}],
          <<"status">> := true}->
            {reply, {Total, Online}, State};
        Reply ->
            {reply, Reply, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%% -----------------------------------------------------------------
%%% handle_cast/2
%%% -----------------------------------------------------------------

handle_cast({post, Action, Cmd, Data}, State)->
    Act = filename:join(["/api/v1/", Action]),
    Reply = post(Act, Cmd, Data),
    ?DEBUG("Post Reply: ~p", [Reply]),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%% -----------------------------------------------------------------
%%% handle_info/2
%%% -----------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%% -----------------------------------------------------------------
%%% terminate/2
%%% -----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%% -----------------------------------------------------------------
%%% code_change/3
%%% -----------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% =================================================================
%%% Internal functions
%%% =================================================================

%%% -----------------------------------------------------------------
%%% post/3 Data = #{}
%%% -----------------------------------------------------------------
post(Action, Cmd, Data) ->
    Bin = jiffy:encode(Data),
    Data1 = binary_to_list(Bin),
    UrlAddr = sim_agent_config:get(url, "http://localhost:8080"),
    Url = lists:concat([UrlAddr, Action]),
    Type = "application/x-www-form-urlencoded",
    Body = encode(Cmd , Data1),
    case httpc:request(post, {Url, [], Type, Body}, [{timeout, 5000}], []) of
        {ok, {{"HTTP/1.1", 200,"OK"}, _Header, Ret}}->
            jiffy:decode(Ret, [return_maps]);
        Other ->
            Other
    end.

%%% -----------------------------------------------------------------
%%% encode/2
%%% -----------------------------------------------------------------
encode(Cmd, Data)->
    Md5Key = sim_agent_config:get(md5key, "dfsa@!7sd&*%$==a3f+yf8-"),
    Timestamp = integer_to_list(sim_agent_misc_lib:unixtime()),
    Vals = [Cmd,  "sim_agent", Timestamp, Data, Md5Key],
    PropList = [{"cmd", Cmd},
                {"reqid", "sim_agent"},
                {"t", Timestamp},
                {"data", http_uri:encode(Data)},
                {"sign",  sim_agent_misc_lib:hex_md5(Vals)}],
    KeyList = proplists:get_keys(PropList),
    make_args(KeyList, PropList).

%%% -----------------------------------------------------------------
%%% make_args/2
%%% -----------------------------------------------------------------
make_args(KeyList, PropList)->
    make_args(KeyList, PropList, []).

%%% -----------------------------------------------------------------
%%% make_args/3
%%% -----------------------------------------------------------------
make_args([], _List, Acc)->
    string:join(Acc, "&");
make_args([Key |T], List, Acc)->
    Value = proplists:get_value(Key, List),
    make_args(T, List, [string:join([Key, Value], "=")| Acc]).