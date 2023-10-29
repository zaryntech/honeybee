-module(script_api).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([get_script/1]).

get_script(ID) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#script{id = ID, _ = '_'})
            end),
    case Res of
        {atomic, []} -> script_not_exist;
        {atomic, [Script]} -> Script;
        _ -> error
    end.

get_script_redeemer(ID) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#script_redeemer{id = ID, _ = '_'})
            end),
    case Res of
        {atomic, []} -> script_not_exist;
        {atomic, [ScriptRedeemer]} -> ScriptRedeemer;
        _ -> error
    end.