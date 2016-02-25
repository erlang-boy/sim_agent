%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-24
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_tcp
%%% =================================================================

-module(sim_agent_tcp).
-author("Liu HuiDong").
-include("types.hrl").
%%% =================================================================
%%% API functions
%%% =================================================================

-export([send_company/2,
         recv_company/2,
         send_agent/2,
         recv_agent/1,
         send_packet/2,
         recv_packet/1,
         handle_packet/3,
         async_recv/3,
         close_sock/1,
         connect/2]).

%%% -----------------------------------------------------------------
%%% send_company/2
%%% -----------------------------------------------------------------
send_company(Sock, Company) ->
    Rand = sim_agent_misc_lib:random(0, 1024 * 1024),
    Now = random_now(),
    ComId = Company#company.id,

    Info = #{
        <<"protocol">>  => <<"noob">>,
        <<"version">>   => <<"1.0.0">>,
        <<"compress">>  => <<"">>,
        <<"status">>    => 0
    },
    Bin = jiffy:encode(Info),
    InfoLen = byte_size(Bin),
    Text = <<Rand:32, Now:32, ComId:?COMPANY_LEN/binary, InfoLen:16, Bin/binary>>,

    PubKey = Company#company.pubkey,
    RsaKey = sim_agent_crypto:rsa_decode_key(PubKey),
    Cipher = sim_agent_crypto:rsa_encrypt(Text, RsaKey),
    Length = byte_size(Cipher) + ?COMPANY_LEN,
    DataSend = <<Length:32, ComId:?COMPANY_LEN/binary, Cipher/binary>>,
    ok = gen_tcp:send(Sock, DataSend).

%%% -----------------------------------------------------------------
%%% recv_company/2
%%% -----------------------------------------------------------------
recv_company(Sock, Company) ->
    {ok, Cipher} = recv_packet(Sock),
    PubKey = Company#company.pubkey,
    RsaKey = sim_agent_crypto:rsa_decode_key(PubKey),
    Text = sim_agent_crypto:rsa_decrypt(Cipher, RsaKey),

    ComId = Company#company.id,
    <<_Rand:32, _Now:32, ComId:?COMPANY_LEN/binary, Rc4Key/binary>> = Text,
    sim_agent_crypto:init_crypto(rc4, [{key, Rc4Key}]).

%%% -----------------------------------------------------------------
%%% send_agent/2
%%% -----------------------------------------------------------------
send_agent(Sock, AgentId) ->
    ServerInfo = dummy_data:server_info(),
    ServerInfoBin = jiffy:encode(ServerInfo),
    InfoLen = byte_size(ServerInfoBin),
    Text = <<AgentId/binary, InfoLen:16, ServerInfoBin/binary>>,
    Cipher = sim_agent_crypto:encrypt(Text),
    ok = gen_tcp:send(Sock, pack(Cipher)).

%%% -----------------------------------------------------------------
%%% recv_agent/1
%%% -----------------------------------------------------------------
recv_agent(Sock) ->
    {ok, Cipher} = recv_packet(Sock),
    Text = sim_agent_crypto:decrypt(Cipher),
    <<AgentId:16/binary>> = Text,
    AgentId.

%%% -----------------------------------------------------------------
%%% send_packet/2
%%% -----------------------------------------------------------------
send_packet(Sock, {Type, From, To, Data}) ->
    DataBin = jiffy:encode(Data),
    Bin = pack(Type, From, To, DataBin),
    Cipher = sim_agent_crypto:encrypt(Bin),
    ok = gen_tcp:send(Sock, pack(Cipher)).

%%% -----------------------------------------------------------------
%%% recv_packet/1
%%% -----------------------------------------------------------------
recv_packet(Sock) ->
    case prim_inet:recv(Sock, 4) of
        {ok, <<Len:32>>} when Len > 0 andalso Len =< ?MAX_PACKET_LEN ->
            case prim_inet:recv(Sock, Len) of
                {ok, Data} -> {ok, Data};
                Error -> Error
            end;
        {ok, <<Len:32>>} -> {error, {invalid_packet_len, Len}};
        Error -> Error
    end.

%%% -----------------------------------------------------------------
%%% handle_packet/3
%%% 获取packet长度
%%% -----------------------------------------------------------------
handle_packet(length, <<Length:32>>, State) ->
    case (Length > 0 andalso Length =< ?MAX_PACKET_LEN) of
        true ->
            async_recv(Length, content, State);
        %% 包长度出现问题
        false ->
            {error, sim_agent:on_logout(State)}
    end;

%%% -----------------------------------------------------------------
%%% async_recv/3
%%% 进行逻辑协议
%%% -----------------------------------------------------------------
handle_packet(content, Bin, State) ->
    <<ContentLength:32, Header:12/binary, Body/binary>>
    = sim_agent_crypto:decrypt(Bin),
    case ContentLength == (byte_size(Header) + byte_size(Body)) of
        %% 解码成功，这里仅仅对长度校验
        true ->
            <<Type:32, From:32, To:32>> = Header,
            Body2 = case Body =:= <<"ping">> of
                        true -> Body;
                        false ->
                            jiffy:decode(Body, [return_maps])
                    end,
            sim_agent_msg:handle_msg(Type, From, To, Body2, State),
            async_recv(4, length, State);
        %% 解码出现问题
        false ->
            sim_agent:on_logout(State),
            {error, State#state{sock = undefined, heartbeat_ref = undefined}}
    end.

%%% -----------------------------------------------------------------
%%% async_recv/3
%%% -----------------------------------------------------------------
async_recv(Len, Callback, State = #state{sock = Sock}) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Len, -1) of
        {ok, Ref} ->
            {ok, State#state{recv_ref = Ref, callback = Callback}};
        Error ->
            ?LOG("async recv: ~p~n", [Error]),
            sim_agent:on_logout(State),
            {error, State#state{sock = undefined, heartbeat_ref = undefined}}
    end.

%%% -----------------------------------------------------------------
%%% close_sock/1
%%% -----------------------------------------------------------------
close_sock(Sock) when is_port(Sock) -> gen_tcp:close(Sock);
close_sock(_) -> ok.

%%% -----------------------------------------------------------------
%%% connect/2
%%% -----------------------------------------------------------------
connect(Ip, Port) ->
    gen_tcp:connect(Ip, Port, [binary, {active, false}]).

%%% =================================================================
%%% Internal functions
%%% =================================================================

%%% -----------------------------------------------------------------
%%% pack/1
%%% |--len(32)--|--data--|
%%% -----------------------------------------------------------------
pack(Data) ->
    Size = byte_size(Data),
    [<<Size:32>>, Data].

%%% -----------------------------------------------------------------
%%% pack/4
%%% |--len(32)--|--type--|--from--|--to--|--body--|
%%% -----------------------------------------------------------------
pack(Type, From, To, Body) ->
    ContentLen = 12 + byte_size(Body),
    [<<ContentLen:32>>, <<Type:32>>, <<From:32>>, <<To:32>>, Body].

%%% -----------------------------------------------------------------
%%% random_now/0
%%% -----------------------------------------------------------------
random_now() ->
    Now = sim_agent_misc_lib:unixtime(),
    case sim_agent_misc_lib:random(1, 2) of
        1 ->
            Now + sim_agent_misc_lib:random(0, 600);
        2 ->
            Now - sim_agent_misc_lib:random(0, 600)
    end.