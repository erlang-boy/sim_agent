%%%---------------------------------------------------
%%% @Module   : msg_library
%%% @Author   : wubx
%%% @Email    : thinear@gmail.com
%%% @Created  : 14-11-1
%%% @Description :
%%%---------------------------------------------------
-module(msg_library).

-export([msg/1]).

-include("msg_type.hrl").

msg(?C2S_HEARTBEAT) ->
    {?C2S_HEARTBEAT, ?MOD_NETWORK, ?MOD_SERVER, #{<<"tc">> => sim_agent_misc_lib:unixtime()}};

msg(?SH_ERROR_REPORT) ->
    {?SH_ERROR_REPORT, ?MOD_SERVER, ?MOD_SERVER, #{
        <<"retCode">>   => 1,
        <<"retMsg">>    => <<"firewall add error">>,
        <<"msgName">>   => <<"">>,
        <<"file">>      => <<"file.c">>,
        <<"function">>  => <<"getter">>,
        <<"lineno">>    => 100,
        <<"extra">>     => <<"esd">>
    }};

msg(?SH_RULES_CHANGED) ->
    {?SH_RULES_CHANGED, ?MOD_FIREWALL, ?MOD_SERVER, #{
        <<"ret_code">>  => 0,
        <<"ret_msg">>   => <<"firewall changed">>,
        <<"data">>      => [
            #{
                <<"new">> => #{
                    <<"type">>  => 1,
                    <<"id">>    => 10001,
                    <<"chain">> => <<"in">>
                },
                <<"old">> => #{
                    <<"type">>  => 1,
                    <<"id">>    => 10001,
                    <<"chain">> => <<"in">>
                }
            }
        ]
    }}.
