%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-3-2
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_config
%%% =================================================================

-module(sim_agent_config).
-author("Liu HuiDong").

-behaviour(gen_server).
-include("sim_agent.hrl").

%%% =================================================================
%%% API functions
%%% =================================================================

-export([load/1,
         set/2,
         get/1, get/2]).

-export([start_link/0]).

%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%% -----------------------------------------------------------------
%%% load/1
%%% -----------------------------------------------------------------
load(Files) ->
    gen_server:call(?MODULE, {load_files, Files}).

%%% -----------------------------------------------------------------
%%% set/2
%%% -----------------------------------------------------------------
set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).

%%% -----------------------------------------------------------------
%%% get/1
%%% -----------------------------------------------------------------
get(Key) ->
    case gen_server:call(?MODULE, {get, Key}) of
        {ok, Value} ->
            Value;
        undefined ->
            erlang:error("Missing configuration key", [Key])
    end.

%%% -----------------------------------------------------------------
%%% get/2
%%% -----------------------------------------------------------------
get(Key, Default) ->
    case gen_server:call(?MODULE, {get, Key}) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.

%%% -----------------------------------------------------------------
%%% Starts the server
%%% -----------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% =================================================================
%%% gen_server callbacks
%%% =================================================================

%%% -----------------------------------------------------------------
%%% init/1
%%% -----------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%% -----------------------------------------------------------------
%%% handle_call/3
%%% -----------------------------------------------------------------
handle_call({load_files, FileNames}, _From, State) ->
    set_keys_from_files(FileNames),
    {reply, ok, State};
handle_call({set, Key, Value}, _From, State) ->
    application:set_env(sim_agent, Key, Value),
    {reply, ok, State};
handle_call({get, Key}, _From, State) ->
    Value = application:get_env(sim_agent, Key),
    {reply, Value, State}.

%%% -----------------------------------------------------------------
%%% handle_cast/2
%%% -----------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.

%%% -----------------------------------------------------------------
%%% handle_info/2
%%% -----------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%% -----------------------------------------------------------------
%%% terminate/2
%%% -----------------------------------------------------------------
terminate(_Reason, _State) ->
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
%%% set_keys_from_files/1
%%% -----------------------------------------------------------------
set_keys_from_files(Files) ->
    KVs = [
        case file:consult(File) of
            {ok, Terms} ->
                Terms;
            {error, Reason} ->
                ?ERROR("Failed to parse config file ~s: ~p\n", [File, Reason]),
                throw(invalid_config),
                notokay
        end || File <- Files ],
    FlatKVs = lists:flatten(KVs),
    [application:set_env(sim_agent, Key, Value) || {Key, Value} <- FlatKVs].
