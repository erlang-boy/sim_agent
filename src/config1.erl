%%%---------------------------------------------------
%%% @Module   : config1
%%% @Author   : wubx
%%% @Email    : thinear@gmail.com
%%% @Created  : 14-11-1
%%% @Description :
%%%---------------------------------------------------
-module(config1).

-export([config/1]).

config(login_shock) ->
    0;
config(relogin) ->
    true;
config(relogin_shock) ->
    30000;
config(force_logout) ->
    true;
config(force_logout_shock) ->
    600000;
config(random_send) ->
    false;
config(send_msg_delay) ->
    5000000;
config(heartbeat) ->
    60000;
config(host) ->
    random_host().

random_host() ->
    Idx = sim_agent_misc_lib:random(1, host_size()),
    host_address(Idx).

host_address(1) ->
    {"192.168.199.4", 7788};
host_address(2) ->
    {"192.168.199.4", 7789}.

host_size() -> 2.