%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-24
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_misc_lib
%%% =================================================================

-module(sim_agent_misc_lib).
-author("Liu HuiDong").

%%% =================================================================
%%% API functions
%%% =================================================================
-export([exponential/1]).
-export([sleep/1,
         sleep/2]).
-export([hex_md5/1]).
-export([unixtime/0,
         longunixtime/0]).
-export([random/2,
         random_string_lowercase/1,
         random_string_upercase/1,
         random_string/2]).
-export([monitor_process/1]).
-export([local_time_format/0]).
-export([time_format/1]).
-export([flush/1]).
-export([urlencode/1]).

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                         (C >= $0 andalso C =< $9) orelse
                         (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse C =:= $_))).

%%% -----------------------------------------------------------------
%%% exponential/1
%%% -----------------------------------------------------------------
exponential(Lambda) ->
    -math:log(random:uniform()) / Lambda.

%%% -----------------------------------------------------------------
%%% sleep/1
%%% -----------------------------------------------------------------
sleep(T) ->
    receive
    after T -> ok
    end.

%%% -----------------------------------------------------------------
%%% sleep/2
%%% -----------------------------------------------------------------
sleep(T, F) ->
    receive
    after T -> F()
    end.

%%% -----------------------------------------------------------------
%%% unixtime/0
%%% 取得当前的unix时间戳
%%% -----------------------------------------------------------------
unixtime() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.

%%% -----------------------------------------------------------------
%%% longunixtime/0
%%% 获取当前的unix毫秒
%%% -----------------------------------------------------------------
longunixtime() ->
    {M, S, Ms} = erlang:now(),
    M * 1000000000 + S * 1000 + Ms div 1000.

%%% -----------------------------------------------------------------
%%% random/2
%%% 随机产生Min~Max
%%% -----------------------------------------------------------------
random(Max, Max) -> Max;
random(Min, Max) ->
    case get("rand_seed") of
        undefined ->
            RandSeed = get_seed(),
            random:seed(RandSeed),
            put("rand_seed", RandSeed);
        _ -> skip
    end,
    M = Min - 1,
    random:uniform(Max - M) + M.

%%% -----------------------------------------------------------------
%%% random_string_lowercase/1
%%% -----------------------------------------------------------------
random_string_lowercase(Len) ->
    random_string(Len, "abcdefghijklmnopqrstuvwxyz0123456789").

%%% -----------------------------------------------------------------
%%% random_string_upercase/1
%%% -----------------------------------------------------------------
random_string_upercase(Len) ->
    random_string(Len, "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").

%%% -----------------------------------------------------------------
%%% random_string/2
%%% -----------------------------------------------------------------
random_string(Len, AllowChars) ->
    Chrs = list_to_tuple(AllowChars),
    ChrsSize = size(Chrs),
    case get("rand_seed") of
        undefined ->
            RandSeed = get_seed(),
            random:seed(RandSeed),
            put("rand_seed", RandSeed);
        _ -> skip
    end,
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).

%%% -----------------------------------------------------------------
%%% get_seed/0
%%% -----------------------------------------------------------------
get_seed() ->
    random:seed(erlang:now()),
    {random:uniform(99999), random:uniform(999999), random:uniform(999999)}.

%% --------------------------------------------------------------------
%% hex_md5/1
%% Md5 algorithm update and improve performance
%% --------------------------------------------------------------------
hex_md5(S) ->
    hex(binary_to_list(erlang:md5(S)), []).

%%% -----------------------------------------------------------------
%%% hex/2
%%% -----------------------------------------------------------------
hex([], Res) ->
    lists:reverse(Res);
hex([N | Ns], Res) ->
    hex(Ns, [int_to_hex(N rem 16),
             int_to_hex(N div 16) | Res]).

%%% -----------------------------------------------------------------
%%% int_to_hex/1
%%% -----------------------------------------------------------------
int_to_hex(D) when (D >= 0) and (D < 10) ->
    D + 48;
int_to_hex(D) ->
    D + 87.

%%% -----------------------------------------------------------------
%%% monitor_process/1
%%% -----------------------------------------------------------------
monitor_process(Pid) ->
    Mref = erlang:monitor(process, Pid),
    receive
        {'DOWN', _Mref, process, Pid, Reason} ->
            {error, Reason}
    after 0 ->
        {ok, Mref}
    end.

%%% -----------------------------------------------------------------
%%% local_time_format/1
%%% -----------------------------------------------------------------
local_time_format() ->
    time_format(erlang:localtime()).

%%% -----------------------------------------------------------------
%%% time_format/1
%%% -----------------------------------------------------------------
time_format({{Y, M, D}, {Ho, Mi, Se}}) ->
    PadZero = fun(Int) -> string:right(integer_to_list(Int), 2, $0) end,
    lists:flatten([
                      integer_to_list(Y), $-,
                      PadZero(M), $-,
                      PadZero(D), " ",
                      PadZero(Ho), $:,
                      PadZero(Mi), $:,
                      PadZero(Se)
                  ]).

%%% -----------------------------------------------------------------
%%% flush/1
%%% -----------------------------------------------------------------
flush(Msg) ->
    receive
        Msg -> true
    after 0 ->
        true
    end.

%%% -----------------------------------------------------------------
%%% @spec urlencode([{Key, Value}]) -> string()
%%% @doc URL encode the property list.
%%% -----------------------------------------------------------------
urlencode(Props) ->
    Pairs = append(Props),
    string:join(Pairs, "&").

%%% -----------------------------------------------------------------
%%% append/1
%%% -----------------------------------------------------------------
append([{K, V} | Rest]) ->
    [quote_plus(K) ++ "=" ++ quote_plus(V) | append(Rest)];
append([]) ->
    [].

%%% -----------------------------------------------------------------
%%% @spec quote_plus(atom() | integer() | string() | binary()) -> string()
%%% @doc URL safe encoding of the given term.
%%% -----------------------------------------------------------------
quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(Binary) when is_binary(Binary) ->
    quote_plus(binary_to_list(Binary));
quote_plus(String) ->
    quote_plus(String, []).

%%% -----------------------------------------------------------------
%%% handle_msg/5
%%% -----------------------------------------------------------------
quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

%%% -----------------------------------------------------------------
%%% handle_msg/5
%%% -----------------------------------------------------------------
hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).
