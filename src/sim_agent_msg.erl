%%% =================================================================
%%% @author Liu HuiDong
%%% @date  16-2-24
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to sim_agent_msg
%%% =================================================================

-module(sim_agent_msg).
-author("Liu HuiDong").

-include("msg_type.hrl").
-include("types.hrl").

%%% =================================================================
%%% API functions
%%% =================================================================
-export([handle_msg/5,
       dispatch_msg/4]).

%%% -----------------------------------------------------------------
%%% handle_msg/5
%%% -----------------------------------------------------------------
handle_msg(_Type, _From, _To, <<"ping">>, _State) ->
     ok;
handle_msg(Type, From, To, Data, State) ->
    print_msg(Type, From, To, Data),
    case ets:lookup(?MAP, To) of
        []->
            ?LOG("not supported mod:~p~n", [To]);
        [#map_table{mod = Mod}] ->
            case catch dispatch_msg(Type, Mod, Data, State) of
                ok -> ok;
                _Err -> ok
            end
    end.

%%% -----------------------------------------------------------------
%%% dispatch_msg/4
%%% -----------------------------------------------------------------
dispatch_msg(Type, Mod, Data, State) ->
    case erlang:function_exported(Mod, msg, 3) of
        true ->
            Mod:msg(Type, Data, State);
        false->
            ?LOG("~p:msg/3 is not exported~n",[Mod])
    end.

%%% =================================================================
%%% Internal functions
%%% =================================================================

%%% -----------------------------------------------------------------
%%% print_msg/4
%%% -----------------------------------------------------------------
print_msg(Type, From, To, Body) ->
    ?LOG("~n type: ~p"
         "~n from: ~p"
         "~n to: ~p"
         "~n body: ~p~n",
         [Type, From, To, Body]).
