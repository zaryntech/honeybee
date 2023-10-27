-module(pool).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([insert/0, pool_history/0, pool_metadata/2, pool_relay/0, pool_delegator/0,
pool_update/0]).

insert() -> 
    Fun = fun() ->
        PoolID = bech32_id:generate(35),
        HexID = hex_id:generate(12),
        Pool = #pool{
            pool_id = PoolID,
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
        PoolHistory = pool_history(),
        PoolMetadata = pool_metadata(PoolID, HexID),
        PoolRelay = pool_relay(),
        PoolDelegator = pool_delegator(),
        PoolUpdate = pool_update(),
        PoolID
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

pool_metadata(PoolID, HexID) ->
    Fun = fun() ->
        PoolMetadata = #pool_metadata{
            pool_id = PoolID,
            hex = HexID,
            url = "",
            hash = "",
            ticker = "",
            name = "",
            description = "",
            homepage = ""},
        mnesia:write(PoolMetadata),
        io:fwrite("~p~n", [PoolMetadata])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

pool_relay() ->
    Fun = fun() ->
        PoolRelay = #pool_relay{
            ipv4 = "",
            ipv6 = "",
            dns = "",
            dns_srv = "",
            port = ""},
        mnesia:write(PoolRelay),
        io:fwrite("~p~n", [PoolRelay])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

pool_delegator() ->
    Fun = fun() ->
        PoolDelegator = #pool_delegator{
            address = "",
            live_stake = ""},
        mnesia:write(PoolDelegator),
        io:fwrite("~p~n", [PoolDelegator])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

pool_update() ->
    Fun = fun() ->
        PoolUpdate = #pool_update{
            tx_hash = "",
            cert_index = "",
            action = ""},
        mnesia:write(PoolUpdate),
        io:fwrite("~p~n", [PoolUpdate])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.