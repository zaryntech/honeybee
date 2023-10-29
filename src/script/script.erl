-module(script).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([insert/0, script_redeemer/1]).

insert() ->
    Fun = fun() ->
        ID = nanoid:gen(),
        Script = #script{
            id = ID,
            script_hash = "",
            type = "",
            serialised_size = ""},
        mnesia:write(Script),
        io:fwrite("~p~n", [Script]),
        script_redeemer(ID),
        ID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

script_redeemer(ID) ->
    Fun = fun() ->
        ScriptRedeemer = #script_redeemer{
            id = ID,
            tx_hash = "",
            tx_index = "",
            purpose = "",
            redeemer_data_hash = "",
            unit_mem = "",
            unit_steps = "",
            fee = ""},
        mnesia:write(ScriptRedeemer),
        io:fwrite("~p~n", [ScriptRedeemer]),
        ID
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.