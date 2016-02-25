%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-24
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to types
%%% =================================================================


-record(company, {
    id,
    name,
    pubkey
}).

-record(agent, {
    id,
    pid,
    online
}).

-record(map_table, {
    id,
    mod
}).

-record(state, {
    company,
    agent_id,
    config,

    sock = undefined,
    recv_ref,
    callback,

    heartbeat_ref = undefined
}).

-define(AGENT, agent).
-define(MAP, map_table).
-define(MAP_FILENAME, "msg_map.config").
-define(MAX_PACKET_LEN, 64 * 1024).
-define(RECONNECT_TIME, infinity).

-define(COMPANY_LEN, 20).

-define(LOG_OK, 1).
-ifdef(LOG_OK).
-define(LOG(F, A), io:format(F, A)).
-else.
-define(LOG(F, A), ok).
-endif.