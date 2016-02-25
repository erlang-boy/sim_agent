%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-24
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent
%%% =================================================================

-module(sim_agent).
-author("Liu HuiDong").

-include("msg_type.hrl").
-include("types.hrl").

-behaviour(gen_fsm).

%% gen_fsm callbacks
-export([init/1,
         logout/3,
         login/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).

%%% =================================================================
%%% API
%%% =================================================================
-export([start_link/3]).
-export([agent_login/1, agent_logout/1]).
-export([start_agent/3, stop_agent/1]).

%%% -----------------------------------------------------------------
%%% agent_login/1
%%% -----------------------------------------------------------------
agent_login(Pid) ->
    gen_fsm:sync_send_event(Pid, login).

agent_logout(Pid)->
    gen_fsm:sync_send_event(Pid, logout).
%%% -----------------------------------------------------------------
%%% start_agent/3
%%% -----------------------------------------------------------------
start_agent(Company, AgentId, Config) ->
    sim_agent_sup:start_child(Company, AgentId, Config).

%%% -----------------------------------------------------------------
%%% stop_agent/1
%%% -----------------------------------------------------------------
stop_agent(Pid) ->
    sim_agent_sup:stop_child(Pid).

%%% -----------------------------------------------------------------
%%% start_link/3
%%% -----------------------------------------------------------------
start_link(Company, AgentId, Config) ->
    gen_fsm:start_link(?MODULE, [Company, AgentId, Config], []).

%%% =================================================================
%%% gen_fsm callbacks
%%% =================================================================

%%% -----------------------------------------------------------------
%%% init/1
%%% -----------------------------------------------------------------
init([Company, AgentId, Config]) ->
    State = #state{
        company = Company,
        agent_id = AgentId,
        config = Config
    },
    agent_login(self()),
    {ok, logout, State}.

%%% -----------------------------------------------------------------
%%% logout/3
%%% -----------------------------------------------------------------
logout(login, _From, State) ->
    {NextS, State1} = login(State),
    {reply, {ok, NextS}, NextS, State1}.

login(logout, _From, State)->
    on_logout(State),
    {reply, {ok, logout}, logout, State#state{sock = undefined,
                                               heartbeat_ref = undefined}}.
%%% -----------------------------------------------------------------
%%% handle_event/3
%%% -----------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%% -----------------------------------------------------------------
%%% handle_sync_event/4
%%% -----------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%% -----------------------------------------------------------------
%%% handle_info/3
%%% -----------------------------------------------------------------
handle_info({inet_async, Sock, Ref, {ok, Data}}, StateName, State = #state{
    sock = Sock, recv_ref = Ref, callback = Callback}) ->
    case sim_agent_tcp:handle_packet(Callback, Data, State) of
        {ok, State1} ->
            {next_state, StateName, State1};
        {error, State1} ->
            {next_state, logout, State1}
    end;
handle_info({inet_async, Sock, Ref, {error, _Reason}},
            _StateName, State = #state{sock = Sock, recv_ref = Ref}) ->
    on_logout(State),
    {next_state, logout,  State#state{sock = undefined,
                                      heartbeat_ref = undefined}};
handle_info(heartbeat, StateName, State = #state{sock = Sock}) when is_port(Sock) ->
    case catch sim_agent_tcp:send_packet(Sock, msg_library:msg(?C2S_HEARTBEAT)) of
        ok ->
            Config = State#state.config,
            Ref = erlang:send_after(Config:config(heartbeat), self(), heartbeat),
            {next_state, StateName, State#state{
                heartbeat_ref = Ref
            }};
        _ ->
            on_logout(State),
            {next_state, logout,  State#state{sock = undefined,
                                              heartbeat_ref = undefined}}
    end;
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%% -----------------------------------------------------------------
%%% terminate/3
%%% -----------------------------------------------------------------
terminate(_Reason, _StateName, #state{agent_id = AgentId,
                                      sock = undefined,
                                      heartbeat_ref = undefined}) ->
    ets:delete(?AGENT, AgentId),
    ok;
terminate(_Reason, _StateName, #state{agent_id = AgentId, sock = Sock} = State) ->
    ets:delete(?AGENT, AgentId),
    sim_agent_tcp:close_sock(Sock),
    sim_agent_crypto:clear(),
    cancel_heartbeat(State#state.heartbeat_ref),
    ok.

%%% -----------------------------------------------------------------
%%% code_change/4
%%% -----------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%% =================================================================
%%% Internal functions
%%% =================================================================

%%% -----------------------------------------------------------------
%%% login/1
%%% -----------------------------------------------------------------
login(State) ->
    Config = State#state.config,
    {Ip, Port} = Config:config(host),
    case sim_agent_tcp:connect(Ip, Port) of
        {ok, Sock} ->
            case catch login(Sock, State) of
                {ok, State1} ->
                    {login, State1};
                Error ->
                    ?LOG("login error: ~p~n", [Error]),
                    on_logout(State),
                    {logout, State#state{sock = undefined,
                                         heartbeat_ref = undefined}}
            end;
        {error, Reason} ->
            ?LOG("login failed: ~p~n", [Reason]),
            on_logout(State),
            {logout, State#state{sock = undefined, heartbeat_ref = undefined}}
    end.

%%% -----------------------------------------------------------------
%%% login/2
%%% -----------------------------------------------------------------
login(Sock, State) ->
    AgentId = handshake(Sock, State),
    on_login(State#state{agent_id = AgentId, sock = Sock}).

%%% -----------------------------------------------------------------
%%% handshake/2
%%% -----------------------------------------------------------------
handshake(Sock, State) ->
    Company = State#state.company,
    DummyId = State#state.agent_id,
    sim_agent_tcp:send_company(Sock, Company),
    sim_agent_tcp:recv_company(Sock, Company),
    login_shock(State#state.config),
    sim_agent_tcp:send_agent(Sock, DummyId),
    sim_agent_tcp:recv_agent(Sock).

%%% -----------------------------------------------------------------
%%% login_shock/1
%%% -----------------------------------------------------------------
login_shock(Config) ->
    Shock = Config:config(login_shock),
    case is_number(Shock) andalso Shock > 0 of
        true ->
            RandomShock = sim_agent_misc_lib:random(1000, Shock),
            timer:sleep(RandomShock);
        false -> ignore
    end.

%%% -----------------------------------------------------------------
%%% on_login/1
%%% -----------------------------------------------------------------
on_login(State = #state{config = Config}) ->
    ?LOG("time: ~p, login: ~p~n",
         [sim_agent_misc_lib:unixtime(), State#state.agent_id]),
    Agent = #agent{
        id = State#state.agent_id,
        pid = self(),
        online = true},
    ets:insert(?AGENT, Agent),
%%% 心跳
    Ref = erlang:send_after(Config:config(heartbeat), self(), heartbeat),
    sim_agent_tcp:async_recv(4, length, State#state{heartbeat_ref = Ref}).

%%% -----------------------------------------------------------------
%%% on_logout/1
%%% -----------------------------------------------------------------
on_logout(State = #state{sock = Sock}) ->
    ?LOG("logout: ~p~n", [State#state.agent_id]),
    Agent = #agent{
        id = State#state.agent_id,
        pid = self(),
        online = false
    },
    ets:insert(?AGENT, Agent),
    sim_agent_tcp:close_sock(Sock),
    sim_agent_crypto:clear(),
    cancel_heartbeat(State#state.heartbeat_ref).

%%% -----------------------------------------------------------------
%%% cancel_heartbeat/1
%%% -----------------------------------------------------------------
cancel_heartbeat(Ref) when is_reference(Ref) -> erlang:cancel_timer(Ref);
cancel_heartbeat(_) -> ok.