-module(network).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([insert/0, network_supply/0, network_stake/0]).

insert() ->
    Fun = fun() ->
        Network = #network{
            supply = network_supply(),
            stake = network_stake()},
        mnesia:write(Network),
        io:fwrite("~p~n", [Network])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

network_supply() ->
    Fun = fun() ->
        NetworkSupply = #network_supply{
            max = "",
            total = "",
            circulating = "",
            locked = "",
            treasury = "",
            reserves = ""},
        mnesia:write(NetworkSupply),
        io:fwrite("~p~n", [NetworkSupply])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

network_stake() ->
    Fun = fun() ->
        NetworkStake = #network_stake{
            live = "",
            active = ""},
        mnesia:write(NetworkStake),
        io:fwrite("~p~n", [NetworkStake])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.