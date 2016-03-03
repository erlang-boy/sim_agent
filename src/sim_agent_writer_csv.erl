%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-27
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_stats_writer_csv
%%% =================================================================

-module(sim_agent_writer_csv).

-export([new/1,
         terminate/1,
         process_summary/5,
         report_error/3,
         report_latency/7]).

-include("sim_agent.hrl").

%%% -----------------------------------------------------------------
%%% @spec new/1
%%% -----------------------------------------------------------------
new(Ops) ->
    ?INFO("module=~s event=start stats_sink=csv\n", [?MODULE]),
    [erlang:put({csv_file, X}, op_csv_file(X)) || X <- Ops],
    SummaryFmt = <<"elapsed, window, total, successful, failed\n">>,
    SummaryFile = write_file("summary.csv", SummaryFmt),
    ErrorsFile = write_file("errors.csv", <<"\"error\",\"count\"\n">>),
    {SummaryFile, ErrorsFile}.

%%% -----------------------------------------------------------------
%%% @spec terminate/1
%%% -----------------------------------------------------------------
terminate({SummaryFile, ErrorsFile}) ->
    ?INFO("module=~s event=stop stats_sink=csv\n", [?MODULE]),
    [ok = file:close(F) || {{csv_file, _}, F} <- erlang:get()],
    ok = file:close(SummaryFile),
    ok = file:close(ErrorsFile),
    ok.

%%% -----------------------------------------------------------------
%%% @spec process_summary/5
%%% -----------------------------------------------------------------
process_summary({SummaryFile, _ErrorsFile}, Elapsed, Window, Oks, Errors) ->
    file:write(SummaryFile, io_lib:format("~w, ~w, ~w, ~w, ~w\n",
                                          [Elapsed,
                                           Window, Oks + Errors,
                                           Oks,
                                           Errors])).

%%% -----------------------------------------------------------------
%%% @spec report_error/3
%%% -----------------------------------------------------------------
report_error({_SummaryFile, ErrorsFile}, Key, Count) ->
    file:write(ErrorsFile, io_lib:format("\"~w\",\"~w\"\n", [Key, Count])).

%%% -----------------------------------------------------------------
%%% @spec report_latency/7
%%% -----------------------------------------------------------------
report_latency({_SummaryFile, _ErrorsFile}, Elapsed, Window, Op,
               Stats, Errors, Units) ->
    Bool = proplists:get_value(n, Stats) > 0,
    Line = line(Bool, Stats, Elapsed, Window, Units, Errors),
    file:write(erlang:get({csv_file, Op}), Line).

%%% =================================================================
%%% Internal functions
%%% =================================================================

%%% -----------------------------------------------------------------
%%% line/5
%%% -----------------------------------------------------------------
line(true, Stats, Elapsed, Window, Units, Errors) ->
    P = proplists:get_value(percentile, Stats),
    Min = proplists:get_value(min, Stats),
    Mean = proplists:get_value(arithmetic_mean, Stats),
    Median = proplists:get_value(median, Stats),
    V95 = proplists:get_value(95, P),
    V99 = proplists:get_value(99, P),
    V999 = proplists:get_value(999, P),
    Max = proplists:get_value(max, Stats),
    io_lib:format("~w, ~w, ~w, ~w, ~.1f, ~w, ~w, ~w, ~w, ~w, ~w\n",
                  [Elapsed, Window, Units, Min, Mean,
                   Median, V95, V99, V999, Max, Errors]);
line(false, _Stats, Elapsed, Window, _Units, Errors) ->
    io_lib:format("~w, ~w, 0, 0, 0, 0, 0, 0, 0, 0, ~w\n",
                  [Elapsed,
                   Window,
                   Errors]).

%%% -----------------------------------------------------------------
%%% op_csv_file/1
%%% -----------------------------------------------------------------
op_csv_file({Label, _Op}) ->
    Fname = normalize_label(Label) ++ "_latencies.csv",
    Fmt = <<"elapsed, window, n, min, mean, median, 95th, 99th, 99_9th, max, errors\n">>,
    write_file(Fname, Fmt).

%%% -----------------------------------------------------------------
%%% normalize_label/1
%%% -----------------------------------------------------------------
normalize_label(Label) when is_list(Label) ->
    replace_special_chars(Label);
normalize_label(Label) when is_binary(Label) ->
    normalize_label(binary_to_list(Label));
normalize_label(Label) when is_integer(Label) ->
    normalize_label(integer_to_list(Label));
normalize_label(Label) when is_atom(Label) ->
    normalize_label(atom_to_list(Label));
normalize_label(Label) when is_tuple(Label) ->
    Parts = [normalize_label(X) || X <- tuple_to_list(Label)],
    string:join(Parts, "-").

%%% -----------------------------------------------------------------
%%% replace_special_chars/1
%%% -----------------------------------------------------------------
replace_special_chars([H | T]) when
    (H >= $0 andalso H =< $9) orelse
    (H >= $A andalso H =< $Z) orelse
        (H >= $a andalso H =< $z) ->
    [H | replace_special_chars(T)];
replace_special_chars([_ | T]) ->
    [$- | replace_special_chars(T)];
replace_special_chars([]) ->
    [].

%%% -----------------------------------------------------------------
%%% get_file_name/1
%%% -----------------------------------------------------------------
write_file(Name, Format) ->
    ResultsDir = sim_agent_config:get(file_id),
    FileName = filename:join([ResultsDir, Name]),
    {ok, F} = file:open(FileName, [raw, binary, write]),
    file:write(F, Format),
    F.
