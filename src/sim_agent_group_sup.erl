%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-27
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_group_sup
%%% =================================================================

-module(sim_agent_group_sup).
-author("Liu HuiDong").

-behaviour(supervisor).
-include("sim_agent.hrl").

%%% =================================================================
%%% API functions
%%% =================================================================
-export([start_link/0,
         get_workers/0,
         start_workers/1,
         stop_workers/1,
         stop_child/1]).

%%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, brutal_kill, Type, [I]}).

%%% -----------------------------------------------------------------
%%% @spec get_workers/0
%%% -----------------------------------------------------------------
get_workers() ->
    Workers = supervisor:count_children(?MODULE),
    {proplists:get_value(workers, Workers), proplists:get_value(active, Workers)}.

%%% -----------------------------------------------------------------
%%% @spec stop_workers/1
%%% -----------------------------------------------------------------
stop_workers(N) ->
    WorkList = supervisor:which_children(?MODULE),
    PidList = [Pid || {_Id, Pid, worker, [sim_agent_worker]} <- WorkList],
    stop_worker(N, PidList).

%%% -----------------------------------------------------------------
%%% @spec start_workers/1
%%% -----------------------------------------------------------------
start_workers(N) ->
    case sim_agent_config:get(rate) of
        Rate when is_number(Rate) ->
            MeanArrival = 1000 / Rate,
            ?INFO("Starting ~w ms/login fixed rate work on ~p\n", [MeanArrival, node()]),
            [begin
                 supervisor:start_child(?MODULE, [?MODULE, N1]),
                 Time = sim_agent_misc_lib:exponential(1 / MeanArrival),
                 sim_agent_misc_lib:sleep(trunc(Time))
             end
             || N1 <- lists:seq(1, N)],ok;
        max ->
            ?INFO("Starting max rate work on ~p\n", [node()]),
            [begin
                 supervisor:start_child(?MODULE, [?MODULE, N1])
             end
             || N1 <- lists:seq(1, N)],ok
    end.

%%% -----------------------------------------------------------------
%%% Starts the supervisor
%%% -----------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%% =================================================================
%%% Supervisor callbacks
%%% =================================================================

%%% -----------------------------------------------------------------
%%% init/1
%%% -----------------------------------------------------------------
init([]) ->
    SupFlags = {simple_one_for_one, 0, 1},
    ChildSpec = [?CHILD(sim_agent_worker, worker)],
    {ok, {SupFlags, ChildSpec}}.

%%% =================================================================
%%% Internal functions
%%% =================================================================

%%% -----------------------------------------------------------------
%%% stop_worker/2
%%% -----------------------------------------------------------------
stop_worker(0, _T) -> ok;
stop_worker(N, [H | T]) when is_pid(H) ->
    stop_child(H),
    stop_worker(N - 1, T).

%%% -----------------------------------------------------------------
%%% stop_child/1
%%% -----------------------------------------------------------------
stop_child(Id) ->
    supervisor:terminate_child(?MODULE, Id).