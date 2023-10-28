-module(epoch).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([insert/0, epoch_parameters/0, epoch_stake/0, epoch_stake_pool/0]).

insert() -> 
    Fun = fun() ->
        Epoch = #epoch{
            epoch = "",
            start_time = "",
            end_time = "",
            first_block_time = "",
            last_block_time = "",
            block_count = "",
            tx_count = "",
            output = "",
            fees = "",
            active_stake = ""},
        mnesia:write(Epoch),
        io:fwrite("~p~n", [Epoch])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

epoch_parameters() ->
    Fun = fun() ->
        EpochParams = #epoch_parameters{
            epoch = 0,
            min_fee_a = "",
            min_fee_b = "",
            max_block_size = "",
            max_tx_size = "",
            max_block_header_size = "",
            key_deposit = "",
            pool_deposit = "",
            e_max = "",
            n_opt = "",
            a0 = "",
            rho = "",
            tau = "",
            decentralisation_param = "",
            extra_entropy = "",
            protocol_major_ver = "",
            protocol_minor_ver = "",
            min_utxo = "",
            min_pool_cost = "",
            nonce = "",
            cost_models = "",
            price_mem = "",
            price_step = "",
            max_tx_ex_mem = "",
            max_tx_ex_steps = "",
            max_block_ex_mem = "",
            max_block_ex_steps = "",
            max_val_size = "",
            collateral_percent = "",
            max_collateral_inputs = "",
            coins_per_utxo_size = ""},
        mnesia:write(EpochParams),
        io:fwrite("~p~n", [EpochParams])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

epoch_stake() ->
    Fun = fun() ->
        EpochStake = #epoch_stake{
            stake_address = "",
            pool_id = "",
            amount = ""},
        mnesia:write(EpochStake),
        io:fwrite("~p~n", [EpochStake])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

epoch_stake_pool() ->
    Fun = fun() ->
        EpochStakePool = #epoch_stake_pool{
            stake_address = "",
            amount = ""},
        mnesia:write(EpochStakePool),
        io:fwrite("~p~n", [EpochStakePool])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.