%%%---------------------------------------------------
%%% @Module   : framework_msg
%%% @Author   : wubx
%%% @Email    : thinear@gmail.com
%%% @Created  : 14-11-12
%%% @Description :
%%%---------------------------------------------------
-module(framework_msg).

-export([msg/3]).

-include("msg_type.hrl").
-include("types.hrl").

msg(?S2C_PLUGIN_LIST, Data, #state{sock=Sock}) ->
    ReqId = maps:get(<<"req_id">>, Data),
    Mods = maps:get(<<"modules">>, Data),
    Type = maps:get(<<"type">>, Data),
    LoopF = fun(Mod, Acc0) ->
        Id = maps:get(<<"id">>, Mod),
        Name = maps:get(<<"name">>, Mod),
        Url = maps:get(<<"url">>, Mod),
        send_plugin_start(Sock, Id, Name),
        Ret = #{
            <<"id">> => Id,
            <<"name">>  => Name,
            <<"url">>   => Url,
            <<"ret_code">> => 0,
            <<"ret_msg">> => <<"load plugin suc">>
        },
        [Ret|Acc0]
    end,
    Rets = lists:foldl(LoopF, [], Mods),
    Msg = # {
        <<"req_id">> => ReqId,
        <<"type">> => Type,
        <<"modules">> => Rets
    },
    sim_agent_tcp:send_packet(Sock, {?C2S_FRAMEWORK_PLUGIN_RESPONSE, ?MOD_FRAMEWORK, ?MOD_SERVER, Msg}),
    ok;

msg(_Type, _Data, _State) ->
    ok.

send_plugin_start(Sock, Id, Name) ->
    Msg = # {
        <<"id">>    => Id,
        <<"name">>  => Name,
        <<"status">>    => 0
    },
    sim_agent_tcp:send_packet(Sock, {?C2S_PLUGIN_STATUS, ?MOD_FRAMEWORK, ?MOD_SERVER, Msg}),
    ok.
