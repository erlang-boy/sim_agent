%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-27
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_worker
%%% =================================================================

-module(sim_agent_worker).
-behaviour(gen_server).

%%% =================================================================
%%% API functions
%%% =================================================================
-export([start_link/2,
         header/1,
         content/2]).

%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("sim_agent.hrl").
-include("msg_type.hrl").
-define(MAX_PACKET_LEN, 64 * 1024).

%%% -----------------------------------------------------------------
%%% @spec start_link/1
%%% -----------------------------------------------------------------
start_link(SupChild, Id) ->
    gen_server:start_link(?MODULE, [SupChild, Id], []).

%%% -----------------------------------------------------------------
%%% @spec header/1
%%% -----------------------------------------------------------------
header(Pid) ->
    gen_server:cast(Pid, header).

%%% -----------------------------------------------------------------
%%% @spec content/2
%%% -----------------------------------------------------------------
content(Pid, Len) ->
    gen_server:cast(Pid, {content, Len}).

%%% =================================================================
%%% @specgen_server callbacks
%%% =================================================================

%%% -----------------------------------------------------------------
%%% init/1
%%% -----------------------------------------------------------------
init([SupChild, Id]) ->
    process_flag(trap_exit, true),
    Driver = sim_agent_config:get(driver),
    {Ip, Port} = random_host(),
    Start = os:timestamp(),
    case catch sim_agent_tcp:connect(Ip, Port) of
        {ok, Sock} ->
            {Result, ElapsedUs} = worker_login(Driver, Sock, Start),
            handle_login_result(Result,
                                ElapsedUs,
                                #session{id = Id, driver = Driver,
                                          sock = Sock, parent_pid = self(),
                                          sup_id = SupChild});
        {error, Reason} ->
            ?ERROR("connect error:~p", [Reason]),
            ElapsedUs = erlang:max(0, timer:now_diff(os:timestamp(), Start)),
            sim_agent_stats:op_complete({login_fail, login_fail},
                                        ok, ElapsedUs),
            sim_agent_stats:op_complete({login_fail, login_fail},
                                        {error, Reason}, ElapsedUs),
            {stop, Reason}
    end.

%%% -----------------------------------------------------------------
%%% handle_call/3
%%% -----------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%% -----------------------------------------------------------------
%%% handle_cast/2
%%% -----------------------------------------------------------------
handle_cast(header, State = #session{sock = Sock}) ->
    case sim_agent_tcp:async_recv(4, length, Sock) of
        {ok, Ref, Callback} ->
            {noreply,
             State#session{recv_ref = Ref,
                           callback = Callback,
                           last_event_time = os:timestamp(),
                           start_recv_time = os:timestamp()}};
        {error, Reason} ->
            {stop, Reason, State}
    end;

handle_cast({content, Len}, State = #session{sock = Sock}) ->
    case sim_agent_tcp:async_recv(Len, content, Sock) of
        {ok, Ref, Callback} ->
            {noreply, State#session{recv_ref = Ref,
                                    callback = Callback,
                                    last_event_time = os:timestamp(),
                                    start_recv_time = os:timestamp()}};
        {error, Reason} ->
            {stop, Reason, State}
    end.

%%% -----------------------------------------------------------------
%%% handle_info/2
%%% -----------------------------------------------------------------
handle_info({inet_async, Sock, Ref, {ok, Data}}, State = #session{
    driver = Driver, sock = Sock, recv_ref = Ref,
    callback = Callback, start_recv_time = Start}) ->
    ElapsedUs = erlang:max(0, timer:now_diff(os:timestamp(), Start)),
    handle_packet(Callback, Data, Driver, Sock, ElapsedUs),
    {noreply, State#session{ last_event_time = os:timestamp()}};
handle_info({inet_async, Sock, Ref, {error, Reason}},
            State = #session{sock = Sock, recv_ref = Ref}) ->
    {stop, Reason, State};
handle_info(heartbeat, State = #session{sock = Sock}) when is_port(Sock) ->
    case catch sim_agent_tcp:send_packet(Sock, msg_library:msg(?C2S_HEARTBEAT)) of
        ok ->
            Ref = erlang:send_after(sim_agent_config:get(heartbeat), self(), heartbeat),
            {noreply, State#session{heartbeat_ref = Ref,
                                    last_event_time = os:timestamp()}};
        {Type, Reason} ->
            ?ERROR("heartbeat error, ~p:~p",[Type, Reason]),
            {stop, Reason, State}
    end;
handle_info({'EXIT', Pid, Reason}, State) ->
    ?ERROR("Worker ~p exited with ~p~n", [Pid, Reason]),
    {stop, shutdown, State}.

%%% -----------------------------------------------------------------
%%% terminate/2
%%% -----------------------------------------------------------------
terminate(normal, State)->
    sim_agent_tcp:close_sock(State#session.sock),
    ok;
terminate(Reason, State = #session{
    last_event_time = Start}) ->
    sim_agent_tcp:close_sock(State#session.sock),
    ElapsedUs = erlang:max(0, timer:now_diff(os:timestamp(), Start)),
    sim_agent_stats:op_complete({login_fail, login_fail}, {error, Reason}, ElapsedUs),
    ok.

%%% -----------------------------------------------------------------
%%% code_change/3
%%% -----------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% =================================================================
%%% Internal functions
%%% =================================================================

%%% -----------------------------------------------------------------
%%% random_host/0
%%% -----------------------------------------------------------------
random_host() ->
    Idx = sim_agent_misc_lib:random(1, host_size()),
    host_address(Idx).

%%% -----------------------------------------------------------------
%%% host_address/1
%%% -----------------------------------------------------------------
host_address(1) ->
    sim_agent_config:get(host1, {"192.168.199.4", 7788});
host_address(2) ->
    sim_agent_config:get(host2, {"192.168.199.4", 7789}).

%%% -----------------------------------------------------------------
%%% host_size/0
%%% -----------------------------------------------------------------
host_size() -> 2.

%%% -----------------------------------------------------------------
%%% handle_packet/4
%%% 获取packet长度
%%% -----------------------------------------------------------------
handle_packet(length, <<Length:32>>, _Driver, _Sock, _ElapsedUs) ->
    case (Length > 0 andalso Length =< ?MAX_PACKET_LEN) of
        true ->
            content(self(), Length);
        %% 包长度出现问题
        false ->
            {error, decode_packet}
    end;

%%% -----------------------------------------------------------------
%%% handle_packet/5
%%% 进行逻辑协议
%%% -----------------------------------------------------------------
handle_packet(content, Bin, Driver, Sock, ElapsedUs) ->
    <<ContentLength:32, Header:12/binary, Body/binary>>
    = sim_agent_crypto:decrypt(Bin),
    case ContentLength == (byte_size(Header) + byte_size(Body)) of
        %% 解码成功，这里仅仅对长度校验
        true ->
            {Type, From, To, Body2} = sim_agent_tcp:decode_content(Header, Body),
            %%% 收包后应答，应答时记录一个操作
            handle_body({Type, From, To, Body2}, Driver, Sock, ElapsedUs),
            header(self());
        false ->
            %% 解码出现问题
            {error, decode_packet}
    end.

%%% -----------------------------------------------------------------
%%% handle_body/4
%%% -----------------------------------------------------------------
handle_body({_Type, _From, _To, <<"ping">>}, _Driver, _Sock, _ElapseUs) ->
    ok;
handle_body({Type, From, To, Body2}, Driver, Sock, _ElapsedUs) ->
    case catch Driver:handle_msg(Type, From, To, Body2) of
        {reply, List} ->
            [ok = sim_agent_tcp:send_packet(Sock, H) || H <- List];
        noreply -> ok;
        {error, Error} ->
            {error, Error}
    end.

%%% -----------------------------------------------------------------
%%% worker_login/3
%%% -----------------------------------------------------------------
worker_login(Driver, Sock, Start) ->
    Result = (catch Driver:login(Sock)),
    ElapsedUs = erlang:max(0, timer:now_diff(os:timestamp(), Start)),
    {Result, ElapsedUs}.

%%% -----------------------------------------------------------------
%%% handle_login_result/3
%%% -----------------------------------------------------------------
%%% 登陆程序有问题时
handle_login_result({'EXIT', Reason}, ElapsedUs, State) ->
    sim_agent_stats:op_complete({login_fail, login_fail}, ok, ElapsedUs),
    sim_agent_stats:op_complete({login_fail, login_fail}, {error, crash}, ElapsedUs),
    sim_agent_tcp:close_sock(State#session.sock),
    ?DEBUG("Driver ~p crashed: ~p\n", [State#session.driver, Reason]),
    {stop, Reason};
%%% 不计录登陆时间
handle_login_result({silent, Sock, AgentId, Company}, _ElapsedUs, State) ->
    Ref = erlang:send_after(sim_agent_config:get(heartbeat), self(), heartbeat),
    header(self()),
    {ok, State#session{sock = Sock, agent_id = AgentId,
                       last_event_time = os:timestamp(),
                       company = Company, heartbeat_ref = Ref}};
%%% 登陆成功
handle_login_result({ok, Sock, AgentId, Company}, ElapsedUs, State) when is_port(Sock) ->
    sim_agent_stats:op_complete({login_succ, login_succ}, ok, ElapsedUs),
    Ref = erlang:send_after(sim_agent_config:get(heartbeat), self(), heartbeat),
    header(self()),
    {ok, State#session{sock = Sock, agent_id = AgentId,
                       last_event_time = os:timestamp(),
                       company = Company, heartbeat_ref = Ref}};
%%%% 登陆失败
handle_login_result({error, Reason}, ElapsedUs, _State) ->
    sim_agent_stats:op_complete({login_fail, login_fail}, ok, ElapsedUs),
    sim_agent_stats:op_complete({login_fail, login_fail}, {error, Reason}, ElapsedUs),
    {stop, Reason}.
