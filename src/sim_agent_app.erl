%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-24
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_app
%%% =================================================================

-module(sim_agent_app).
-author("Liu HuiDong").

-behaviour(application).

-include("types.hrl").

%%% =================================================================
%%% API functions
%%% =================================================================
-export([start/2,
         stop/1]).
-export([reload/0]).

%%% -----------------------------------------------------------------
%%% reload/0
%%% -----------------------------------------------------------------
reload()->
    Dir = code:priv_dir(sim_agent),
    FileName = filename:join(Dir, ?MAP_FILENAME),
    case file:consult(FileName) of
        {ok, Terms} ->
            ets:insert(map_table, Terms);
        {error, Reason}->
            exit(Reason)
    end.

%%% -----------------------------------------------------------------
%%% start/2
%%% -----------------------------------------------------------------
start(_StartType, _StartArgs) ->
    ets:new(?AGENT, [set, public, named_table, {keypos, #agent.id}]),
    ets:new(?MAP, [set, public, named_table, {keypos, #map_table.id}]),
    reload(),
    case sim_agent_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%% -----------------------------------------------------------------
%%% stop/1
%%% -----------------------------------------------------------------
stop(_State) ->
    ok.
