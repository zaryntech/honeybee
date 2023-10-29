-module(network).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([insert/0, network_supply/1, network_stake/1]).

insert() ->
    Fun = fun() ->
        ID = nanoid:gen(),
        Network = #network{
            id = ID,
            supply = network_supply(ID),
            stake = network_stake(ID)},
        mnesia:write(Network),
        io:fwrite("~p~n", [Network]),
        ID 
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

network_supply(ID) ->
    Fun = fun() ->
        NetworkSupply = #network_supply{
            id = ID,
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

network_stake(ID) ->
    Fun = fun() ->
        NetworkStake = #network_stake{
            id = ID,
            live = "",
            active = ""},
        mnesia:write(NetworkStake),
        io:fwrite("~p~n", [NetworkStake])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.