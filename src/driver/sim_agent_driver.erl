%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-25
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_driver
%%% =================================================================

-module(sim_agent_driver).
-author("Liu HuiDong").

%%% =================================================================
%%% API functions
%%% =================================================================
-export([login/1,
         handle_msg/4]).

-include("sim_agent.hrl").

-define(UPDATE_AGENT,                16#01000015).  % 升级agent
-define(RECV_UPDATE_AGENT,           16#01000016).  % 升级返回
-define(GET_SCRIPT,                  16#000000019). % get script
-define(DH_SHELLAUDIT,     16#1a000000).
-define(SCRIPT_STATUS,     16#1a000001).
-define(BASH_NOT_FOUND,  11).

%%% -----------------------------------------------------------------
%%% @spec login/1
%%% @return {ok, Sock, AgentId, Company}
%%%         | {silent, Sock, AgentId1, Company}
%%%         | {error, Reason}
%%% -----------------------------------------------------------------
login(Sock) ->
    Company = dummy_data:random_company(),
    AgentId = <<"0000000000000000">>,
    sim_agent_tcp:send_company(Sock, Company),
    sim_agent_tcp:recv_company(Sock, Company),
    sim_agent_tcp:send_agent(Sock, AgentId),
    AgentId1 = sim_agent_tcp:recv_agent(Sock),
    {ok, Sock, AgentId1, Company}.

%%% -----------------------------------------------------------------
%%% @spec handle_msg/5  {reply, [{Type, To, From, Body}]} | noreply
%%% -----------------------------------------------------------------
%%% 登陆完毕后，应答升级
handle_msg(?UPDATE_AGENT, From, To, #{<<"req_id">> := ReqId,
                                    <<"url">> := Url})->
    {reply, [{?RECV_UPDATE_AGENT, To, From, #{<<"req_id">> =>ReqId,
                                              <<"ret_code">> => 1,
                                              <<"ret_msg">> => <<"don't need update agent">>,
                                              <<"url">> => Url}},
             {?GET_SCRIPT, To, From, #{<<"ret_code">> => 1, <<"ret_msg">> => []}},
             {?SCRIPT_STATUS,  ?DH_SHELLAUDIT, ?DH_SHELLAUDIT, #{<<"ret_code">> => ?BASH_NOT_FOUND}}]};

handle_msg(Type, From, To, Body)->
    ?DEBUG("Type:~p, From:~p, To:~p, Body:~p", [to_hex(Type),
                                               to_hex(From),
                                               to_hex(To), Body]),
    case sim_agent_config:get(Type, common) of
         common->
             noreply;
         Mod ->
             dispatch(Mod, Type, From, To, Body)
    end.

%%% -----------------------------------------------------------------
%%% dispatch/5
%%% -----------------------------------------------------------------
dispatch(Mod, Type, From, To, Body)->
    case erlang:function_exported(Mod, msg, 2) of
        true ->
            Mod:msg(Type, From, To, Body);
        false ->
            ?WARN("function msg of ~p is not exported !", [Mod]),
            noreply
    end.

%%% -----------------------------------------------------------------
%%% to_hex/1
%%% -----------------------------------------------------------------
to_hex(B) when is_number(B)->
    integer_to_list(B, 16).