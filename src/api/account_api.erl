-module(account_api).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([get_account/1, get_pool/1, get_pool_id/1, get_controlled_amount/1, get_rewards/1,
get_history/1, get_delegations/1]).

% Get the Account Info by Stake Address
get_account(StakeAddress) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#account{stake_address = StakeAddress, _ = '_'})
            end),
    case Res of
        {atomic, []} -> account_not_exist;
        {atomic, [Account]} -> Account;
        _ -> error
    end. 

get_pool(StakeAddress) ->
    Account = get_account(StakeAddress),
    PoolID = Account#account.pool_id,
    Pool = pool_api:get_pool(PoolID),
    Pool.

get_pool_id(StakeAddress) ->
    Account = get_account(StakeAddress),
    PoolID = Account#account.pool_id,
    PoolID.

get_controlled_amount(StakeAddress) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#account{stake_address = StakeAddress, _ = '_'})
            end),
    case Res of
        {atomic, []} -> account_not_exist;
        {atomic, [Account]} -> Account#account.controlled_amount;
        _ -> error
    end. 

get_rewards(StakeAddress) ->
    PoolID = get_pool_id(StakeAddress),
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#account_reward{pool_id = PoolID, _ = '_'})
            end),
    case Res of
        {atomic, []} -> account_not_exist;
        {atomic, [AccountReward]} -> AccountReward;
        _ -> error
    end. 

get_history(StakeAddress) ->
    PoolID = get_pool_id(StakeAddress),
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#account_history{pool_id = PoolID, _ = '_'})
            end),
    case Res of
        {atomic, []} -> account_not_exist;
        {atomic, [AccountHistory]} -> AccountHistory;
        _ -> error
    end. 

get_delegations(StakeAddress) ->
    PoolID = get_pool_id(StakeAddress),
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#account_delegation{pool_id = PoolID, _ = '_'})
            end),
    case Res of
        {atomic, []} -> account_not_exist;
        {atomic, [AccountDelegation]} -> AccountDelegation;
        _ -> error
    end.