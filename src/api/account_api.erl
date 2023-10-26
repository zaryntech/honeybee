-module(account_api).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([get_account/1, get_controlled_amount/1, get_rewards/1]).

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
    Account = get_account(StakeAddress),
    Account#account.rewards_sum.