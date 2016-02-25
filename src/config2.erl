%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-24
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent
%%% =================================================================
-module(config2).

-export([config/1]).

config(login_shock) ->
    application:get_env(sim_app, login_shock, 0);
config(relogin) ->
    application:get_env(sim_app, relogin, false);
config(force_logout) ->
    application:get_env(sim_app, force_logout, false);
config(random_send) ->
    application:get_env(sim_app, random_send, false);
config(send_msg_delay) ->
    application:get_env(sim_app, send_msg_delay, 5000);
config(heartbeat) ->
    application:get_env(sim_app, heartbeat, 60000);
config(host) ->
    random_host().

random_host() ->
    Idx = sim_agent_misc_lib:random(1, host_size()),
    host_address(Idx).

host_address(1) ->
    application:get_env(sim_app, host1, {"192.168.199.4", 7788});
host_address(2) ->
    application:get_env(sim_app, host2, {"192.168.199.4", 7789}).

host_size() -> 2.