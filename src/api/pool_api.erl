-module(pool_api).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([get_pool/1, get_blocks/1, get_delegators/1]).

% Get Pool info by Pool ID
get_pool(PoolID) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#pool{pool_id = PoolID, _ = '_'})
            end),
    case Res of
        {atomic, []} -> pool_not_exist;
        {atomic, [Pool]} -> Pool;
        _ -> error
    end.

get_blocks(PoolID) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#pool{pool_id = PoolID, _ = '_'})
            end),
    case Res of
        {atomic, []} -> pool_not_exist;
        {atomic, [Pool]} -> Pool#pool.blocks_minted;
        _ -> error
    end.

get_delegators(PoolID) ->
    Pool = get_pool(PoolID),
    Delegators = Pool#pool.live_delegators,
    Delegators.