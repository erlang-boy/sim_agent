# sim_agent功能和使用说明
***

## 功能：

    １．可以通过配置，向om的web服务发送后台请求．相关模块为sim_agent_httpc.erl
    ２．通过配置，可以定义消息处理
    ３．可通过配置，设置登陆的速度
    ４．可以通过csv生成图片（需要安装R）

## 使用说明：

### 启动前

    ./rebar clean compile;relx
    
    cd ./_rel/sim_agent 将lib中的sim_agent**中priv目录链接到此处
    把Makefile也移入此处
    
    ./_rel/sim_agent/bin/sim_agent console

### 启动后
    
    sim_agent:start(). %% 启动测试
    
    sim_agent:add(1000). %% 增加1000个agent
    sim_agent:add(1000,max). %% 以最大速度登陆
    
    sim_agent:reduce(1000). %% 减少1000个agent
    
    sim_agent:stop(). %% 停止测试

### 查看数据

   通过另一个shell进入./_rel/sim_agent/中，执行make results,或者直接进入tests目录查看相关数据
   make results为根据数据生成图片