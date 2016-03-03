-module(sim_agent_app).

-behaviour(application).

%% API
-export([start/0,
         stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

-include("sim_agent.hrl").
%% ==================================================================
%% API
%%===================================================================

start() ->
    application:start(sim_agent, permanent).

stop() ->
    application:stop(sim_agent).

%% ===================================================================
%% Application callbacks
%%===================================================================

start(_StartType, _StartArgs) ->
    application:set_env(sasl, sasl_error_logger, {file, "log.sasl.txt"}),
    {ok, Pid} = sim_agent_sup:start_link(),
    {ok, Pid}.

stop(_State) ->
    ok.