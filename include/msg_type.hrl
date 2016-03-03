%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-24
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to msg_type
%%% =================================================================

%% server，所有发送消息from带上
-define(MOD_SERVER,         16#ff000000).
%% 框架
-define(MOD_FRAMEWORK,      16#01000000).
%% 网络模块
-define(MOD_NETWORK,        16#02000000).
%% 文件监控模块
-define(MOD_FILE_MONITOR,   16#05000000).
%% 防火墙模块
-define(MOD_FIREWALL,       16#09000000).

%% 心跳
-define(C2S_HEARTBEAT,      16#ffffffff).

%% 插件命令执行结果
-define(C2S_FRAMEWORK_PLUGIN_RESPONSE, 16#01000009).

-define(C2S_PLUGIN_STATUS, 16#0100000f).

%% 添加可执行文件
-define(C2S_ADD_EXE_FILE,   16#05000201).
%% 删除可执行文件
-define(C2S_DEL_EXE_FILE,   16#05000202).
%% 可执行文件修改
-define(C2S_MODIFY_EXE_FILE, 16#05000203).


-define(S2C_PLUGIN_LIST,          16#01000006).

%% 发送防火墙规则列表
-define(S2C_RESET_FIREWALL_RULES, 16#09000001).
%% 添加一条防火墙规则
-define(S2C_ADD_FIREWALL_RULE,    16#09000006).
%% 删除一条防火墙规则
-define(S2C_DEL_FIREWALL_RULE,    16#09000007).

-define(SH_RULES_CHANGED,         16#09000005).

-define(SH_ERROR_REPORT, 16#ffffff0f).
