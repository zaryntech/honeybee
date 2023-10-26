-module(account).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([insert/0, account_reward/1, account_history/1, account_delegation/1,
account_registration/0]).

insert() ->
    Fun = fun() ->
        Bech32Address = bech32_id:generate(30),
        Pool = pool:insert(),
        Account = #account{
            stake_address = Bech32Address,
            active = true,
            active_epoch = 0,
            controlled_amount = "",
            rewards_sum = "",
            withdrawals_sum = "",
            reserves_sum = "",
            treasury_sum = "",
            withdrawable_amount = "",
            pool_id = Pool},
        mnesia:write(Account),
        io:fwrite("Account Info: ~p~n", [Account]),
        AccountReward = account_reward(Pool),
        AccountHistory = account_history(Pool),
        AccountDelegation = account_delegation(Pool),
        Bech32Address
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

account_reward(Pool) ->
    Fun = fun() ->
        AccountReward = #account_reward{
            epoch = 0,
            amount = "",
            pool_id = Pool,
            type = ""},
        mnesia:write(AccountReward),
        io:fwrite("~p~n", [AccountReward])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

account_history(Pool) ->
    Fun = fun() ->
        AccountHistory = #account_history{
            active_epoch = 0,
            amount = "",
            pool_id = Pool},
        mnesia:write(AccountHistory),
        io:fwrite("~p~n", [AccountHistory])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

account_delegation(Pool) ->
    Fun = fun() ->
        AccountDelegation = #account_delegation{
            active_epoch = 0,
            tx_hash = "",
            amount = "",
            pool_id = Pool},
        mnesia:write(AccountDelegation),
        io:fwrite("~p~n", [AccountDelegation])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

account_registration() ->
    Fun = fun() ->
        AccountRegistration = #account_registration{
            tx_hash = "",
            action = ""},
        mnesia:write(AccountRegistration),
        io:fwrite("~p~n", [AccountRegistration])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.
