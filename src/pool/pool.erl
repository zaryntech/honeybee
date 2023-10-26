-module(pool).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([insert/0, pool_history/0]).

insert() ->
    Fun = fun() ->
        ID = nanoid:gen(),
        HexID = hex_id:generate(12),
        Pool = #pool{
            pool_id = ID,
            hex = HexID,
            vrf_key = "", 
            blocks_minted = "", 
            live_stake = "",
            live_size = "",
            live_saturation = "",
            live_delegators = "",
            active_stake = "", 
            active_size = "",
            declared_pledge = "", 
            live_pledge = "", 
            margin_cost = "", 
            fixed_cost = "", 
            reward_account = "", 
            owners = "", 
            registration = "",
            retirement = ""},
        mnesia:write(Pool),
        io:fwrite("Stake Pool Inserted ~p~n", [Pool]),
        ID 
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

pool_history() ->
    Fun = fun() ->
        PoolHistory = #pool_history{
            epoch = 0,
            blocks = 0,
            active_stake = "",
            active_size = 0,
            delegators_count = 0,
            rewards = "",
            fees = ""},
        mnesia:write(PoolHistory),
        io:fwrite("~p~n", [PoolHistory])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.