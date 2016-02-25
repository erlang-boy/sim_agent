%%%---------------------------------------------------
%%% @Module   : firewall_msg
%%% @Author   : wubx
%%% @Email    : thinear@gmail.com
%%% @Created  : 14-11-12
%%% @Description :
%%%---------------------------------------------------
-module(firewall_msg).

-export([msg/3]).

-include("msg_type.hrl").
-include("types.hrl").

msg(?S2C_RESET_FIREWALL_RULES, Data, #state{sock=Sock}) ->
    ?LOG("recv reset msg~n", []),
    ReqId = maps:get(<<"req_id">>, Data),
    reply(?S2C_RESET_FIREWALL_RULES, ReqId, Sock),
    ok;

msg(?S2C_ADD_FIREWALL_RULE, Data, #state{sock=Sock}) ->
    ?LOG("recv add msg~n", []),
    ReqId = maps:get(<<"req_id">>, Data),
    reply(?S2C_ADD_FIREWALL_RULE, ReqId, Sock),
    ok;

msg(?S2C_DEL_FIREWALL_RULE, Data, #state{sock=Sock}) ->
    ?LOG("recv del msg~n", []),
    ReqId = maps:get(<<"req_id">>, Data),
    reply(?S2C_DEL_FIREWALL_RULE, ReqId, Sock),
    ok;

msg(16#09000008, #{<<"req_id">> := ReqId}, #state{sock=Sock}) ->
    reply(16#09000008, ReqId, Sock),
    ok;

msg(_Type, _Data, _State) -> ok.

reply(Type, ReqId, Sock) ->
    Msg = # {
        <<"req_id">>    => ReqId,
        <<"ret_code">>  => 0
    },
    sim_agent_tcp:send_packet(Sock, {Type, ?MOD_FIREWALL, ?MOD_SERVER, Msg}).
%%     Rand = misc_lib:random(1, 100),
%%     case Rand =< 50 of
%%         true  ->
%%             Msg = # {
%%                 <<"req_id">>    => ReqId,
%%                 <<"ret_code">>  => 0
%%             },
%%             agent:send_packet(Sock, {Type, ?MOD_FIREWALL, ?MOD_SERVER, Msg});
%%         false ->
%%             case Rand =< 80 of
%%                 true  ->
%%                     ErrCode = case Type of
%%                                   ?S2C_RESET_FIREWALL_RULES -> -2008;
%%                                   ?S2C_ADD_FIREWALL_RULE -> -2001;
%%                                   ?S2C_DEL_FIREWALL_RULE -> -2007;
%%                                   _ -> -1
%%                               end,
%%                     Msg = # {
%%                         <<"req_id">>    => ReqId,
%%                         <<"ret_code">>  => ErrCode,
%%                         <<"ret_msg">>   => <<"">>
%%                     },
%%                     agent:send_packet(Sock, {Type, ?MOD_FIREWALL, ?MOD_SERVER, Msg});
%%                 false -> ignore
%%             end
%%     end.
