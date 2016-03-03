%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-27
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_stats
%%% =================================================================

-module(sim_agent_stats).

-behaviour(gen_server).

-include("sim_agent.hrl").

%%% =================================================================
%%% API functions
%%% =================================================================
-export([start_link/0,
         run/0,
         stop/0,
         op_complete/3]).

%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {ops,
                start_time = os:timestamp(),
                last_write_time = os:timestamp(),
                report_interval,
                report_status = stop,
                report_ref,
                errors_since_last_report = false,
                stats_writer_data,
                last_warn = {0, 0, 0}}).

-define(WARN_INTERVAL, 1000).

%%% -----------------------------------------------------------------
%%% @spec run/0
%%% -----------------------------------------------------------------
run() ->
    gen_server:call({global, ?MODULE}, run).

%%% -----------------------------------------------------------------
%%% @spec stop/0
%%% -----------------------------------------------------------------
stop() ->
    gen_server:cast({global, ?MODULE}, stop).

%%% -----------------------------------------------------------------
%%% @spec op_complete/3
%%% -----------------------------------------------------------------
op_complete(Op, ok, ElapsedUs) ->
    op_complete(Op, {ok, 1}, ElapsedUs);
op_complete(Op, {ok, Units}, ElapsedUs) ->
    gen_server:cast({global, ?MODULE}, {Op, {ok, Units}, ElapsedUs});
op_complete(Op, Result, ElapsedUs) ->
    gen_server:call({global, ?MODULE}, {op, Op, Result, ElapsedUs}, infinity).

%%% -----------------------------------------------------------------
%%% Starts the server
%%% -----------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%%% =================================================================
%%% gen_server callbacks
%%% =================================================================

%%% -----------------------------------------------------------------
%%% init/1
%%% -----------------------------------------------------------------
init([]) ->
    %% Trap exits so we have a chance to flush data
    process_flag(trap_exit, true),
    process_flag(priority, high),
    folsom:start(),
    ets:new(sim_agent_errors, [protected, named_table]),
    ets:new(sim_agent_total_errors, [protected, named_table]),
    Ops = [{login_fail, login_fail}, {login_succ, login_succ}],
    Interval = sim_agent_config:get(report_interval, 10),
    [begin
         folsom_metrics:new_histogram({latencies, Op}, slide, Interval),
         folsom_metrics:new_counter({units, Op})
     end || Op <- Ops],
    ReportInterval = timer:seconds(Interval),
    {ok, #state{ops = Ops, report_interval = ReportInterval}}.

%%% -----------------------------------------------------------------
%%% handle_call/3
%%% -----------------------------------------------------------------
handle_call(run, _From, State = #state{report_status = running}) ->
    ?WARN("report status is running", []),
    {reply, ok, State};
handle_call(run, _From, State = #state{ops = Ops, report_status = stop}) ->
    set_path(),
    sim_agent_config:set(report_status, running),
    {ok, Ref} = timer:send_interval(State#state.report_interval, report),
    New = sim_agent_writer_csv:new(Ops),
    Now = os:timestamp(),
    {reply, ok, State#state{report_ref = Ref,
                            report_status = running,
                            stats_writer_data = New,
                            start_time = Now,
                            last_write_time = Now}};
handle_call({op, Op, {error, Reason}, _ElapsedUs}, _From, State) ->
    increment_error_counter(Op),
    increment_error_counter({Op, Reason}),
    {reply, ok, State#state{errors_since_last_report = true}}.

%%% -----------------------------------------------------------------
%%% handle_cast/2
%%% -----------------------------------------------------------------
handle_cast(stop, State = #state{ report_status = stop}) ->
    sim_agent_config:set(report_status, stop),
    ?INFO("report status is stop",[]),
    {noreply, State};
handle_cast(stop, State = #state{report_status = running}) ->
    sim_agent_config:set(report_status, stop),
    stop_writer(State),
    {noreply, State#state{report_ref = undefined, report_status = stop}};
handle_cast({Op, {ok, Units}, ElapsedUs}, State = #state{last_write_time = LWT,
                                                         report_interval = RI}) ->
    Now = os:timestamp(),
    TimeSinceLastReport = timer:now_diff(Now, LWT) / 1000,
    TimeSinceLastWarn = timer:now_diff(Now, State#state.last_warn) / 1000,
    NewState = check_report(TimeSinceLastReport, RI, TimeSinceLastWarn, State, Now),
    folsom_metrics:notify({latencies, Op}, ElapsedUs),
    folsom_metrics:notify({units, Op}, {inc, Units}),
    {noreply, NewState};
handle_cast(_, State) ->
    {noreply, State}.

%%% -----------------------------------------------------------------
%%% handle_info/2
%%% -----------------------------------------------------------------
handle_info(report, State) ->
    consume_report_msgs(),
    Now = os:timestamp(),
    process_stats(Now, State),
    {noreply, State#state{last_write_time = Now, errors_since_last_report = false}}.

%%% -----------------------------------------------------------------
%%% terminate/2
%%% -----------------------------------------------------------------
terminate(_Reason, State = #state{stats_writer_data = Writer_data}) ->
    process_stats(os:timestamp(), State),
    report_total_errors(State),
    sim_agent_writer_csv:terminate(Writer_data).

%%% -----------------------------------------------------------------
%%% code_change/3
%%% -----------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% =================================================================
%%% Internal functions
%%% =================================================================

stop_writer(State)->
    case State#state.report_ref =:= undefined of
        true -> ok;
        false -> timer:cancel(State#state.report_ref)
    end,
    case State#state.stats_writer_data =:= undefined of
        true -> ok;
        false->
            process_stats(os:timestamp(), State),
            report_total_errors(State),
            sim_agent_writer_csv:terminate(State#state.stats_writer_data)
    end.
%%% -----------------------------------------------------------------
%%% set_path/0
%%% -----------------------------------------------------------------
set_path()->
    DefaultResultsDir = "~/workspace/sim_agent/tests",
    ResultsDir = sim_agent_config:get(tests, DefaultResultsDir),
    case sim_agent_config:get(sim_agent_rel_path, undefined) of
        undefined ->
            {ok, Dir} = file:get_cwd(),
            sim_agent_config:set(sim_agent_rel_path, Dir),
            TestDir = test_dir(ResultsDir),
            ok = file:set_cwd(TestDir);
        PDir ->
            TestDir = test_dir(PDir),
            ok = file:set_cwd(TestDir)
    end.

%%% -----------------------------------------------------------------
%%% check_report/5
%%% -----------------------------------------------------------------
check_report(TimeSinceLastReport, RI, TimeSinceLastWarn, State, Now) ->
    Value = TimeSinceLastReport > (RI * 2),
    Value1 = TimeSinceLastWarn > ?WARN_INTERVAL,
    case Value andalso Value1 of
        true ->
            ?WARN("sim_agent_stats has not reported in ~.2f milliseconds\n",
                  [TimeSinceLastReport]),
            {message_queue_len, QLen} = process_info(self(), message_queue_len),
            ?WARN("stats process mailbox size = ~w\n", [QLen]),
            State#state{last_warn = Now};
        false ->
            State
    end.

%%% -----------------------------------------------------------------
%%% increment_error_counter/1
%%% -----------------------------------------------------------------
increment_error_counter(Key) ->
    ets_increment(sim_agent_errors, Key, 1).

%%% -----------------------------------------------------------------
%%% ets_increment/3
%%% -----------------------------------------------------------------
ets_increment(Tab, Key, Incr) when is_integer(Incr) ->
    case catch (ets:update_counter(Tab, Key, Incr)) of
        Value when is_integer(Value) ->
            ok;
        {'EXIT', _} ->
            case ets:insert_new(Tab, {Key, Incr}) of
                true ->
                    ok;
                _ ->
                    ets_increment(Tab, Key, Incr)
            end
    end;
ets_increment(Tab, Key, Incr) when is_float(Incr) ->
    Old = case ets:lookup(Tab, Key) of
              [{_, Val}] -> Val;
              [] -> 0
          end,
    true = ets:insert(Tab, {Key, Old + Incr}).

%%% -----------------------------------------------------------------
%%% error_counter/1
%%% -----------------------------------------------------------------
error_counter(Key) ->
    lookup_or_zero(sim_agent_errors, Key).

%%% -----------------------------------------------------------------
%%% lookup_or_zero/2
%%% -----------------------------------------------------------------
lookup_or_zero(Tab, Key) ->
    case catch (ets:lookup_element(Tab, Key, 2)) of
        {'EXIT', _} ->
            0;
        Value ->
            Value
    end.

%%% -----------------------------------------------------------------
%%% process_stats/2
%%% -----------------------------------------------------------------
process_stats(Now, State = #state{stats_writer_data = Write_data}) ->
    spawn(fun() -> sim_agent:info() end),
    Elapsed = timer:now_diff(Now, State#state.start_time) / 1000000,
    Window = timer:now_diff(Now, State#state.last_write_time) / 1000000,
    {Oks, Errors, OkOpsRes} = fold_and_report(State, Elapsed, Window),
    [folsom_metrics_counter:dec({units, Op}, OpAmount)
     || {Op, OpAmount} <- OkOpsRes],
    sim_agent_writer_csv:process_summary(Write_data, Elapsed,
                                         Window, Oks, Errors),
    case (State#state.errors_since_last_report) of
        true ->
            handle_error(), ok;
        false ->
            ok
    end.

%%% -----------------------------------------------------------------
%%% report_latency/4
%%% -----------------------------------------------------------------
report_latency(#state{stats_writer_data = Write_data}, Elapsed, Window, Op) ->
    Stats = folsom_metrics:get_histogram_statistics({latencies, Op}),
    Units = folsom_metrics:get_metric_value({units, Op}),
    Errors = error_counter(Op),
    sim_agent_writer_csv:report_latency(Write_data,
                                        Elapsed, Window, Op,
                                        Stats, Errors, Units),
    {Units, Errors}.

%%% -----------------------------------------------------------------
%%% report_total_errors/1
%%% -----------------------------------------------------------------
report_total_errors(State) ->
    case ets:tab2list(sim_agent_total_errors) of
        [] ->
            ?INFO("No Errors.\n", []);
        UnsortedErrCounts ->
            ErrCounts = lists:sort(UnsortedErrCounts),
            ?INFO("Total Errors:\n", []),
            F =
            fun({Key, Count}) ->
                is_need_report(false, Key, Count, State)
            end,
            lists:foreach(F, ErrCounts)
    end.

%%% -----------------------------------------------------------------
%%% handle_error/0
%%% -----------------------------------------------------------------
handle_error() ->
    ErrCounts = ets:tab2list(sim_agent_errors),
    true = ets:delete_all_objects(sim_agent_errors),
    ?INFO("report Errors:~p\n", [lists:sort(ErrCounts)]),
    [ets_increment(sim_agent_total_errors, Err, Count) ||
        {Err, Count} <- ErrCounts].

%%% -----------------------------------------------------------------
%%% is_need_report/5
%%% -----------------------------------------------------------------
is_need_report(true, _Key, _Count, _State) -> ok;
is_need_report(false, Key, Count, State) ->
    ?INFO("  ~p: ~p\n", [Key, Count]),
    sim_agent_writer_csv:report_error(State#state.stats_writer_data, Key, Count).

%%% -----------------------------------------------------------------
%%% fold_and_report/3
%%% -----------------------------------------------------------------
fold_and_report(State, Elapsed, Window) ->
    Fun =
    fun(Op, {TotalOks, TotalErrors, OpsResAcc}) ->
        {Oks, Errors} = report_latency(State, Elapsed, Window, Op),
        {TotalOks + Oks, TotalErrors + Errors, [{Op, Oks} | OpsResAcc]}
    end,
    lists:foldl(Fun, {0, 0, []}, State#state.ops).

%%% -----------------------------------------------------------------
%%% consume_report_msgs/0
%%% -----------------------------------------------------------------
consume_report_msgs() ->
    receive
        report ->
            consume_report_msgs()
    after 0 ->
        ok
    end.

%%% -----------------------------------------------------------------
%%% test_dir/1
%%% -----------------------------------------------------------------
test_dir(ResultsDir) ->
    ResultsDirAbs = filename:absname(ResultsDir),
    Name = id(),
    TestDir = filename:join([ResultsDirAbs, Name]),
    sim_agent_config:set(file_id, TestDir),
    {ok, TestDir} = {filelib:ensure_dir(filename:join(TestDir, "foobar")), TestDir},
    Link = filename:join([ResultsDir, "current"]),
    [] = os:cmd(?FMT("rm -f ~s; ln -sf ~s ~s", [Link, TestDir, Link])),
    TestDir.

%%% -----------------------------------------------------------------
%%% id/0
%%% -----------------------------------------------------------------
id() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    ?FMT("~w~2..0w~2..0w_~2..0w~2..0w~2..0w", [Y, M, D, H, Min, S]).