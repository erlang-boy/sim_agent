%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-27
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent
%%% =================================================================

-module(sim_agent).
-author("Liu HuiDong").
-include("sim_agent.hrl").

-define(Fmt, "~n=========================================================~n"
             "~p         cpu  => ~.2f %~n"
             "~p         memory => ~p ~n"
             "~p         processes => ~p ~n"
             "~p         filehandles => ~p ~n"
             "~p         client_workers: ~p,  client_online: ~p~n"
             "~p         sys_workers: ~p,  sys_online: ~p~n"
             "~p         input: ~p,  output: ~p~n"
             "=========================================================~n").
%%% =================================================================
%%% API functions
%%% =================================================================
-export([start/0, start/1, stop/0]).
-export([add/1, add/2, reduce/1]).
-export([info/0]).
-export([test_case/0]).

%%% -----------------------------------------------------------------
%%% @spec start/0
%%% -----------------------------------------------------------------
start() ->
    start("/home/lhd/workspace/sim_agent/conf/test.config").

%%% -----------------------------------------------------------------
%%% @spec start/1
%%% -----------------------------------------------------------------
start(Config) ->
    sim_agent_config:load([Config]),
    case sim_agent_config:get(report_status, stop) of
        stop ->
            ok = sim_agent_stats:run(),
            test_case(),
            spawn(fun() ->
                DurationMins = sim_agent_config:get(duration, 10),
                wait_for_stop(DurationMins)
                  end);
        running ->
            ?INFO("running",[]),
            ok
    end.

%%% -----------------------------------------------------------------
%%% @spec test_case/0
%%% -----------------------------------------------------------------
test_case() ->
    case sim_agent_config:get(tweb, undefined) of
        undefined -> ok;
        {async, Interval, Action, Cmd, Data} ->
            sim_agent_httpc:async_tweb(Action, Cmd, Data),
            timer:apply_after(timer:seconds(Interval), ?MODULE, test_case, []);
        {sync, Interval, Action, Cmd, Data} ->
            Reply = sim_agent_httpc:sync_tweb(Action, Cmd, Data),
            timer:apply_after(timer:seconds(Interval), ?MODULE, test_case, []),
            Reply
    end.

%%% -----------------------------------------------------------------
%%% @spec info/1
%%% -----------------------------------------------------------------
info() ->
    Res = os:cmd("lsof -p" ++ os:getpid() ++ " | wc -l"),
    Handles =
    list_to_integer(string:strip(string:strip(Res, both), both, $\n)),
    Cpu = cpu_sup:util(),
    Mem = erlang:memory(total),
    Processes = length(erlang:processes()),
    {{input, Input}, {output, Output}} = erlang:statistics(io),
    {Total, Active} = case (catch sim_agent_httpc:get_online_agent(<<"default">>)) of
                          {'EXIT', {timeout, _}} -> {httptimeout, httptimeout};
                          Other -> Other
                      end,
    {ClientWorkers, ClientOnline} = sim_agent_group_sup:get_workers(),
    Time = sim_agent_misc_lib:local_time_format(),
    Args = [Time, Cpu,
            Time, to_str(Mem),
            Time, Processes,
            Time, Handles,
            Time, ClientWorkers, ClientOnline,
            Time, Total, Active,
            Time, to_str(Input), to_str(Output)],
    Args1 = io_lib:format(?Fmt, Args),
    case sim_agent_config:get(debug, true) of
        true -> file:write_file("./sys_data.log", Args1, [raw, append]);
        false -> ?INFO(?Fmt, Args)
    end.

%%% -----------------------------------------------------------------
%%% @spec stop/0
%%% -----------------------------------------------------------------
stop() ->
    case sim_agent_config:get(report_status, stop) of
        stop -> ok;
        running ->
            sim_agent_sup:stop_child(sim_agent_group_sup),
            sim_agent_sup:start_child(sim_agent_group_sup, supervisor),
            sim_agent_stats:stop()
    end.

%%% -----------------------------------------------------------------
%%% @spec add/1
%%% -----------------------------------------------------------------
add(WorkerNum) ->
    case sim_agent_config:get(report_status, stop) of
        stop ->
            ?INFO("test is stoped", []),
            ok;
        running ->
            sim_agent_group_sup:start_workers(WorkerNum)
    end.

%%% -----------------------------------------------------------------
%%% @spec add/2
%%% -----------------------------------------------------------------
add(WorkerNum, Rate) ->
    case sim_agent_config:get(report_status, stop) of
        stop ->
            ?INFO("test is stoped", []), ok;
        running ->
            sim_agent_config:set(rate, Rate),
            sim_agent_group_sup:start_workers(WorkerNum)
    end.

%%% -----------------------------------------------------------------
%%% @spec reduce/1
%%% -----------------------------------------------------------------
reduce(WorkerNum) ->
    case sim_agent_config:get(report_status, stop) of
        stop ->
            ?INFO("test is stoped", []), ok;
        running ->
            sim_agent_group_sup:stop_workers(WorkerNum)
    end.
%%% =================================================================
%%% Internal functions
%%% =================================================================

%%% -----------------------------------------------------------------
%%% wait_for_stop/1
%%% -----------------------------------------------------------------
wait_for_stop(DurationMins) ->
    Duration = timer:minutes(DurationMins) + timer:seconds(1),
    sim_agent_misc_lib:sleep(Duration),
    case cpu_sup:util() > 80 of
        true ->
            sim_agent_sup:stop_child(sim_agent_group_sup),
            sim_agent_sup:start_child(sim_agent_group_sup, supervisor),
            sim_agent_stats:stop(),
            make_png(),
            ?CONSOLE("Test completed after ~p mins.\n", [DurationMins]);
        false ->
            make_png(),
            wait_for_stop(DurationMins)
    end.

%%% -----------------------------------------------------------------
%%% make_png/1
%%% -----------------------------------------------------------------
make_png() ->
    TestsDir = sim_agent_config:get(tests),
    Pwd = filename:absname(TestsDir),
    file:set_cwd(Pwd),
    os:cmd("make results"),
    Pwd1 = sim_agent_config:get(file_id),
    file:set_cwd(Pwd1).

%%% -----------------------------------------------------------------
%%% to_str/1
%%% -----------------------------------------------------------------
to_str(B) ->
    KB = B div 1024,
    MB = KB div 1024,
    GB = MB div 1024,
    if
        GB > 10 -> integer_to_list(GB) ++ " GB";
        MB > 10 -> integer_to_list(MB) ++ " MB";
        KB > 0 -> integer_to_list(KB) ++ " kB";
        true -> integer_to_list(B) ++ " B"
    end.