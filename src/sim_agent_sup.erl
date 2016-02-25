%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-24
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_sup
%%% =================================================================

-module(sim_agent_sup).
-author("Liu HuiDong").

-behaviour(supervisor).

-define(SERVER, ?MODULE).

%%% ==================================================================
%%%   API functions
%%% ==================================================================
-export([start_link/0]).
-export([start_child/3,stop_child/1]).
%% Supervisor callbacks
-export([init/1]).

%%% ------------------------------------------------------------------
%%% @spec start_child(Company, AgentId, Config) ->
%%%    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
%%% ------------------------------------------------------------------
start_child(Company, AgentId, Config)->
    supervisor:start_child(?SERVER, [Company, AgentId, Config]).

%%% ------------------------------------------------------------------
%%% @spec stop_child/1
%%% ------------------------------------------------------------------
stop_child(Pid)->
    supervisor:terminate_child(?SERVER, Pid).

%%% ------------------------------------------------------------------
%%% @spec(start_link() ->
%%%    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
%%% ------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%% ==================================================================
%%% Supervisor callbacks
%%% ==================================================================

%%% ------------------------------------------------------------------
%%% @spec(init(Args :: term()) ->
%%%  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
%%%       MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
%%%       [ChildSpec :: supervisor:child_spec()]}} |
%%%                                         ignore | {error, Reason :: term()}).
%%% ------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {'sim_agent', {'sim_agent', start_link, []},
              Restart, Shutdown, Type, ['sim_agent']},

    {ok, {SupFlags, [AChild]}}.
