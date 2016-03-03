%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-27
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_sup
%%% =================================================================

-module(sim_agent_sup).

-behaviour(supervisor).

%%% =================================================================
%%% API functions
%%% =================================================================
-export([start_link/0]).
-export([start_child/2, stop_child/1]).
%% Supervisor callbacks
-export([init/1]).

-include("sim_agent.hrl").

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Prop), {I, {I, start_link, []}, Prop, 5000, Type, [I]}).

%%% -----------------------------------------------------------------
%%% Starts the supervisor
%%% -----------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% -----------------------------------------------------------------
%%% @spec start_child/2
%%% -----------------------------------------------------------------
start_child(Mod, Type)->
    supervisor:start_child(?MODULE, ?CHILD(Mod, Type, temporary)).

%%% -----------------------------------------------------------------
%%% @spec stop_child/1
%%% -----------------------------------------------------------------
stop_child(Mod) ->
    supervisor:terminate_child(?MODULE, Mod),
    supervisor:delete_child(?MODULE, Mod).

%%% =================================================================
%%% Internal functions
%%% =================================================================

%%% -----------------------------------------------------------------
%%% init/1
%%% -----------------------------------------------------------------
init([]) ->
    {ok, {{one_for_one, 5, 10},
        [?CHILD(sim_agent_config, worker),
         ?CHILD(sim_agent_httpc, worker),
         ?CHILD(sim_agent_stats, worker),
         ?CHILD(sim_agent_group_sup, supervisor)]}}.
