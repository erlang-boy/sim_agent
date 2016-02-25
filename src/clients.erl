%%%---------------------------------------------------
%%% @Module   : clients
%%% @Author   : wubx
%%% @Email    : thinear@gmail.com
%%% @Created  : 14-10-30
%%% @Description :
%%%---------------------------------------------------
-module(clients).

-export([start_agent/0]).
-export([start_agent/1]).
-export([start_agents/1]).
-export([stop_all/0]).
-export([stop_agent/1]).

-include("msg_type.hrl").
-include("types.hrl").

start_agent() ->
    start_agent(<<"0000000000000000">>).

start_agent(AgentId) ->
    Config = config2,
    Company = dummy_data:random_company(),
    sim_agent:start_agent(Company, AgentId, Config).

start_agents(0) -> ok;
start_agents(N) when N > 0 ->
    Config = config2,
    Company = dummy_data:random_company(),
    AgentId = <<"0000000000000000">>,
    sim_agent:start_agent(Company, AgentId, Config),
    %% timer:sleep(100),
    start_agents(N-1).

stop_all() ->
    Agents = ets:tab2list(ets_agent),
    Fun = fun(#agent{pid = Pid}) ->
        sim_agent:stop_agent(Pid)
          end,
    lists:foreach(Fun, Agents).

stop_agent(AgentId) ->
    case ets:lookup(ets_agent, AgentId) of
        [#agent{pid=Pid}] ->
            sim_agent:stop_agent(Pid);
        [] -> ok
    end.

