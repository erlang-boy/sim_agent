%% @author Huaqiao <huaqiao.long@ericsson.com>
%% @copyright 2011 & Huaqiao Long

%% @doc Erlang module for automatically reloading modified modules
%%      during development.

-module(load).
-behaviour(gen_server).
-author('huaqiao').
-vsn("1.0").

-compile({parse_transform, ms_transform}).

-export([start_dev/0]).

-export([multi_start/0, multi_start/1,
         start/0, start/1,
         start_link/0, start_link/1]).

-export([stop/0, multi_stop/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% External API
-export([get_state_modules/0, get_modules/1,
         add_path/1,
         test/2, tests/1,
         r_test/2, r_tests/1,
         open/1, test_dump/2, test_dump/3, tests_dump/1, tests_dump/2]).

-export([local/0, local/1, local/2,
         global/0, global/1, global/2,
         is_changed/1, all_loaded_changed/0,
         get_load_dir/0, get_module_dir/1, get_dev_patch_dir/0,
         local_all_loaded_changed/0, global_all_loaded_changed/0,
         l_modules/1,  nl_modules/1]).

-record(state, {tref, mtlist = [], modules = [], handle}).

-define(SECONDS(V), timer:seconds(V)).
-define(LOG(Format, Arg), io:format(user, Format, Arg)).


%% External API

%% @spec multi_start() -> ok
%% @doc Multi nodes start the loader and load modules.
multi_start() ->
    multi_start([]).

%% @spec multi_start(list()) -> ok
%% @doc Multi nodes start the load and load modules.
multi_start(Dir) ->
    case rpc:multicall([node() | nodes()], ?MODULE, start, [Dir]) of
        {Res, []} ->
            Res;
        {_, Faild} ->
            Faild
    end.


%% @spec start_dev() -> ServerRet
%% @doc Start the load server for devlopment.
start_dev() ->
    case sym_env:is_dev() of
        true ->
            start([]);
        false ->
            ignore
    end.

%% @spec start() -> ServerRet
%% @doc Start the load.
start() ->
    start([]).

%% @spec start(list()) -> ServerRet
%% @doc Start the load.
start(Dir) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Dir], []).

%% @spec start_link() -> ServerRet
%% @doc Start the load.
start_link() ->
    start_link([]).

%% @spec start_link(list()) -> ServerRet
%% @doc Start the load.
start_link(Dir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Dir], []).

%% @spec stop() -> stopped
%% @doc Stop the load.
stop() ->
    gen_server:call(?MODULE, stop).

%% @spec multi_stop() -> ok
%% @doc Stop the load.
multi_stop() ->
    gen_server:multi_call([node() | nodes()], ?MODULE, stop).

%% @spec get_state_modules() -> list()
%% @doc get_state_modules.
get_state_modules() ->
    gen_server:call(?MODULE, get_state_modules).

%% @spec open(LogDir) -> ok
%% @doc open logfile
open(LogDir) ->
    gen_server:call(?MODULE, {open_file, LogDir}).

%%
%% gen_server callbacks
%%

%% @spec init([]) -> {ok, State}
%% @doc gen_server init, opens the server in an initial state.
init([Dir]) ->
    process_flag(trap_exit,true),
    {ok, TRef} = timer:send_after(?SECONDS(2), {init_doit, Dir}),
    {ok, #state{tref = TRef, mtlist = orddict:new()}}.

%% @spec handle_call(Args, From, State) -> list()| atom() | bool() | tuple()
%% @doc gen_server callback.
handle_call(stop, _From, State) ->
    {stop, shutdown, stopped, State};
handle_call(get_state_modules, _From, #state{modules = Ms} = State) ->
    {reply, Ms, State};
handle_call({open_file, LogDir}, _From, #state{handle=Fd} = State) ->
    _ = (catch file:close(Fd)),
    Handle = open_file(LogDir),
    {reply, ok, State#state{handle=Handle}};
handle_call(_Req, _From, State) ->
    {reply, {error, badreq}, State}.

%% @spec handle_cast(Cast, State) -> any()
%% @doc gen_server callback.
handle_cast(_Req, State) ->
    {noreply, State}.

%% @spec handle_info(Info, State) -> any()
%% @doc gen_server callback.
handle_info({init_doit, Dir}, #state{tref = TRef} = State) ->
    {ok, cancel} = timer:cancel(TRef),
    Modules1 = get_modules(Dir),
    {ok, State1} = init_doit(Modules1, State),
    {ok, TRef1} = timer:send_after(?SECONDS(2), {loop_doit, Dir}),
    {noreply, State1#state{tref = TRef1}};
handle_info({loop_doit, Dir}, #state{tref = TRef, mtlist = MTList,
                                     modules = Modules} = State) ->
    {ok, cancel} = timer:cancel(TRef),
    Modules1 = get_modules(Dir),
    {ok, State1} = loop_doit(Modules1, MTList, State),
    State2 = reload_old_version(Modules, Modules1, State1),
    {ok, TRef1} = timer:send_after(?SECONDS(2), {loop_doit, Dir}),
    {noreply, State2#state{tref = TRef1}};
handle_info({trace, From, call, {Mod, Fun, Args}},
            #state{handle = Fd} = State) ->
    String = lists:flatten(io_lib:format("(~p) call ~p:~p(~s)~n",
                                         [From, Mod, Fun, fargs(Args)])),
    ok = write(Fd, String),
    {noreply, State};
handle_info({trace, From, return_from, {Mod, Fun, Arith}, Res},
            #state{handle = Fd} = State) ->
    String = lists:flatten(io_lib:format("(~p) return ~p:~p/~p -> ~p~n",
                                         [From, Mod, Fun, Arith, Res])),
    ok = write(Fd, String),
    {noreply, State};
handle_info({'EXIT', _Pid, shutdown}, _State) ->
    %% stop normal from supervisor terminate.
    exit(shutdown);
handle_info(Info, State) ->
    io:format("load: Unknown info:~p~n", [Info]),
    %% write_logfile(Info),
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% @doc gen_server termination callback.
terminate(_Reason, #state{tref = TRef, handle = Fd}) ->
    {ok, cancel} = timer:cancel(TRef),
    file:close(Fd),
    ok.

%% @spec code_change(_OldVsn, State, _Extra) -> State
%% @doc gen_server code_change callback (trivial).
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%% External API

fargs([]) -> [];
fargs([A]) -> io_lib:format("~p", [A]);  %% last arg
fargs([A|Args]) -> [io_lib:format("~p,", [A]) | fargs(Args)];
fargs(A) -> io_lib:format("~p", [A]). % last or only arg

write(Fd, String) ->
    ok = file:write(Fd, [String, $\n]).

open_file([]) ->
    Home = check_exist_dev_patch_dir(),
    ?LOG("LogFile path:~p~n", [Home]),
    open_file([Home, "/dbg.log"], true);
open_file(LogDir) ->
    ?LOG("LogFile path:~p~n", [LogDir]),
    open_file([LogDir, "/dbg.log"], true).

open_file(List, Bool) ->
    FilePath = lists:concat(List),
    ok = backup(FilePath, Bool),
    ?LOG("FilePath:~p~n", [FilePath]),
    {ok, FD} = file:open(FilePath, [raw, read, write, delayed_write, binary, append]),
    FD.

backup(Filename, Bool) ->
    case file:read_file_info(Filename) of
        {ok, _FileInfo} when Bool =:= true ->
            ok = file:rename(Filename, Filename ++ ".bak"),
            ok;
        {ok, _FileInfo} ->
            ok = file:delete(Filename),
            ok;
        {error, _Reason} ->
            ok
    end.

%% @spec get_modules(list()) -> list()
%% @doc get modules.
get_modules([]) ->
    Dir = check_exist_dev_patch_dir(),
    ok = add_path(Dir),
    List = filelib:wildcard(Dir ++ "/*.beam"),
    Modules = [list_to_atom(filename:basename(L, ".beam")) || L <- List],
    Modules;
get_modules(AppDir) ->
    ok = add_path(AppDir ++ "/ebin"),
    List = filelib:wildcard(AppDir ++ "/ebin/*.beam"),
    Modules = [list_to_atom(filename:basename(L, ".beam")) || L <- List],
    Modules.

check_exist_dev_patch_dir() ->
    case get_dev_patch_dir() of
        false ->
            get_load_dir();
        Dir ->
            Dir
    end.

%% @spec local() -> list()
%% @doc In the local node,
%%       load modules in the load path all modules.
local() ->
    local([], []).

%% @spec local(list()) -> list()
%% @doc In the local node,
%%       load modules in the load path all modules and extra include modules.
local(Exclude) ->
    local(Exclude, []).

%% @spec local(list(), list()) -> list()
%% @doc In the local node,
%%       load modules in the dir path all modules and extra include modules.
local(Exclude, Dir) ->
    Modules = get_modules(Dir),
    Loaded = [{M, reload(M)} || M <- (Modules ++ Exclude), is_changed(M)],
    {node(), Loaded}.

%% @spec local_all_loaded_changed() -> list()
%% @doc In the local node,
%%       load all loaded changed modules in the load path all modules.
local_all_loaded_changed() ->
    Changed = all_loaded_changed(),
    [reload(Mod) || {Mod, _Fn} <- Changed],
    ok.

%% @spec global() -> {[global_result()], [atom()]}
%% @doc all sibling nodes and as same the local function.
global() ->
    global([], []).

%% @spec global(list()) -> {[global_result()], [atom()]}
%% @doc all sibling nodes and as same the local function.
global(Exclude) ->
    global(Exclude, []).

%% @spec global(list(), list()) -> {[global_result()], [atom()]}
%% @doc all sibling nodes and as same the local function.
global(Exclude, Dir) ->
    chunked_multicall([node() | nodes()], ?MODULE, local, [Exclude, Dir]).

%% @spec global_all_loaded_changed() -> {[global_result()], [atom()]}
%% @doc all sibling nodes and as same the local function.
global_all_loaded_changed() ->
    chunked_multicall([node() | nodes()], ?MODULE,
                      local_all_loaded_changed, []).

%% @spec chunked_multicall(Nodes, M, F, A) -> {ResL, BadNodes}
%% @doc Perform the function Fun on all siblings using rpc:multicall,
%%      with two nodes at a time.
chunked_multicall([N0, N1 | Rest], M, F, A) ->
    {Pass0, Fail0} = rpc:multicall([N0, N1], M, F, A),
    {Pass1, Fail1} = chunked_multicall(Rest, M, F, A),
    {Pass0 ++ Pass1, Fail0 ++ Fail1};
chunked_multicall(Nodes, M, F, A) ->
    rpc:multicall(Nodes, M, F, A).

%% @spec l_modules([atom()]) -> [{node(), [{module, atom()}]} | {error, term()}]
%% @doc code:purge/1 and code:load_file/1 the given list of modules in order,
%%      return the results of code:load_file/1.
l_modules(Modules) when is_list(Modules) ->
    {node(), [begin code:purge(M), code:load_file(M) end || M <- Modules]}.

%% @spec nl_modules([atom()]) -> [{node(), [{module, atom()}]} | {error, term()}]
%% @doc All sibling nodes and as same function the l_modules function.
nl_modules(Modules) when is_list(Modules) ->
    chunked_multicall([node() | nodes()], ?MODULE, l_modules, [Modules]).

%% @spec is_changed(atom()) -> boolean()
%% @doc true if the loaded module is a beam with a compile time and
%%      does not match the on-disk beam file a compile time, returns false otherwise.
is_changed(M) ->
    try
        compile_time(M:module_info(compile)) =/=
        compile_time(code:get_object_code(M))
    catch _:_ ->
              false
    end.

%% @spec all_loaded_changed() -> [atom()]
%% @doc Return a list of beam modules that have all loaded changed.
all_loaded_changed() ->
    [{M, Fn} || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M)].

%% @spec get_load_dir() -> string()
%% @doc Return the application directory for this application. Equivalent to
%%      get_load_dir(?MODULE).
get_load_dir() ->
    get_module_dir(?MODULE).


%% @spec get_dev_patch_dir() -> string()
%% @doc Return the dev_patches directory
get_dev_patch_dir() ->
    os:getenv("DEV_PATCHES_DIR").

%% @spec get_module_dir(Module) -> string()
%% @doc Return the application directory for Module. It assumes Module is in
%%      a standard OTP layout application in the ebin or src directory.
get_module_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(Here).

%% @spec add_path(list()) -> ok
%% @doc add dir in code search path.
add_path(Dir) ->
    true = code:add_patha(Dir),
    ok.

%% Internal API

compile_time({M, Beam, _Fn}) ->
    {ok,{M, [{compile_info, Info}]}} = beam_lib:chunks(Beam, [compile_info]),
    {_, Time}= lists:keyfind(time, 1, Info),
    Time;
compile_time(Info) when is_list(Info) ->
    {_, Time} = lists:keyfind(time, 1, Info),
    Time.

init_doit(Modules, State) ->
    init_doit(Modules, [], State).

init_doit([], NewMTList, State) ->
    {ok, State#state{mtlist = NewMTList}};
init_doit([M | L], Acc, State) ->
    T = compile_time(code:get_object_code(M)),
    try
        {file, _Fn} = code:is_loaded(M),
        T = compile_time(M:module_info(compile)),
        init_doit(L, [{M, T}|Acc], State)
    catch _:_ ->
              %% Note:Ensure output modules exist,handle add new modules,
              %%      but not loaded in running system.
              reload(M),
              init_doit(L, [{M, T}|Acc], State)
    end.

loop_doit([], NewMTList, State) ->
    {ok, State#state{mtlist = NewMTList}};
loop_doit([M | L], MTList, State) ->
    T = compile_time(code:get_object_code(M)),
    try
        {M, T} = lists:keyfind(M, 1, MTList),
        loop_doit(L, MTList, State)
    catch _:_ ->
              %% Note:Ensure output modules exist,handle add new modules,
              %%      but not loaded in running system.
              reload(M),
              NewMTList = orddict:store(M, T, MTList),
              loop_doit(L, NewMTList, State)
    end.

reload_old_version([], NewMs, State) ->
    State#state{modules = NewMs};
reload_old_version(Ms, Ms, State) ->
    State;
reload_old_version(OldMs, NewMs, #state{mtlist = MTList} = State) ->
    RemoveMs = OldMs -- NewMs,
    NewMTList = remove_mtime(RemoveMs, MTList),
    [begin
         ?LOG("Old version ", []),
         reload(Mod)
     end || Mod <- RemoveMs],
    State#state{mtlist = NewMTList, modules = NewMs}.

remove_mtime([], NewMTList) ->
    NewMTList;
remove_mtime([M | L], MTList) ->
    NewMTList = orddict:erase(M, MTList),
    orddict:erase(L, NewMTList).

reload(Module) ->
    ?LOG("Reloading: ~p ...", [Module]),
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            ?LOG(" ok.~n", []),
            case erlang:function_exported(Module, test, 0) of
                true ->
                    ?LOG(" - Calling ~p:test() ...", [Module]),
                    case catch Module:test() of
                        ok ->
                            ?LOG(" ok.~n", []),
                            reload;
                        Reason ->
                            ?LOG(" fail: ~p.~n", [Reason]),
                            reload_but_test_failed
                    end;
                false ->
                    reload
            end;
        {error, Reason} ->
            ?LOG(" fail: ~p.~n", [Reason]),
            error
    end.

%% @spec test(Module::atom(), Function::atom()) -> {ok, Res}
%%
%% @doc dbg test interface.
test(Mod, Fun) ->
    tests([{Mod, Fun}]).

%% @spec tests(Args::args()) -> {ok, Res}
%% @type args() = [{M, F}]
%% @doc dbg test interface.
tests(Args) when is_list(Args) ->
    case dbg:tracer() of
        {ok, _Pid} ->
            ok;
        {error, already_started} ->
            dbg:stop_clear(),
            dbg:tracer()
    end,
    set_tpl(Args).

%% @spec r_test(Module::atom(), Function::atom()) -> {ok, Res}
%%
%% @doc dbg remote test interface.
r_test(Mod, Fun) ->
    r_tests([{Mod, Fun}]).

%% @spec r_tests(Args::args()) -> {ok, Res}
%% @doc dbg remote test interface.
r_tests(Args) when is_list(Args) ->
    GL = group_leader(),
    case dbg:tracer(process, {fun dbg:dhandler/2, GL}) of
        {ok, _Pid} ->
            ok;
        {error, already_started} ->
            dbg:stop_clear(),
            dbg:tracer(process, {fun dbg:dhandler/2, GL})
    end,
    set_tpl(Args).

test_dump(Mod, Fun) ->
    tests_dump([{Mod, Fun}], []).

test_dump(Mod, Fun, LogDir) ->
    tests_dump([{Mod, Fun}], LogDir).

tests_dump(Args) when is_list(Args) ->
    tests_dump(Args, []).

tests_dump(Args, LogDir) when is_list(Args) ->
    _ = (catch start()),
    ok = open(LogDir),
    Fun = fun(Mess, _State) ->
               {?MODULE, node()} ! Mess
          end,
    case dbg:tracer(process, {Fun, start}) of
        {ok, _Pid2} ->
            ok;
        {error, already_started} ->
            dbg:stop_clear(),
            dbg:tracer(process, {Fun, start})
    end,
    set_tpl(Args).

set_tpl(Args) ->
    dbg:p(all, [c]),
    [case MFs of
         {M, F} ->
             dbg:tpl(M, F, '_', dbg:fun2ms(fun(_) -> return_trace() end));
         MFs ->
             [M|Ls] = tuple_to_list(MFs),
             [dbg:tpl(M, F, '_', dbg:fun2ms(fun(_) -> return_trace() end)) ||
                F <- Ls]
     end || MFs <- Args].

