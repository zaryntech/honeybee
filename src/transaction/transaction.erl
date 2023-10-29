-module(transaction).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([insert/0, calculateHash/1, byte_sizee/1, transaction_stake/0,
transaction_delegation/0, transaction_withdrawal/0, transaction_mir/0,
transaction_pool_update/0, transaction_pool_retire/0, transaction_redeemer/0,
transaction_utxos/0, utxo_input/0, utxo_output/0]).

insert() -> 
    Fun = fun() -> 
        ID = nanoid:gen(),
        Transaction = #transaction{
            id = ID,
            block = 0,
            block_height = 0,
            block_time = 0,
            slot = 0,
            index = 0,
            output_amount = 0,
            fees = "",
            deposit = "",
            invalid_before = 0,
            invalid_hereafter = 0,
            utxo_count = 0,
            withdrawal_count = 0,
            mir_cert_count = 0,
            delegation_count = 0,
            stake_cert_count = 0,
            pool_update_count = 0,
            pool_retire_count = 0,
            asset_mint_or_burn_count = 0,
            redeemer_count = 0
            },
        mnesia:write(Transaction),
        Hash = calculateHash(Transaction),
        Size = byte_sizee(Transaction),
        NewTransaction = Transaction#transaction{hash = Hash, size = Size},
        mnesia:write(NewTransaction),
        io:fwrite(" Transaction Inserted ~p~n", [NewTransaction]),
        UtxoInput = utxo_input(),
        UtxoOutput = utxo_output(),
        Hash 
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

% Calculate the Hash of Block
calculateHash(Block) ->
    Hash = crypto:hash(sha3_512, term_to_binary(Block)),
    HashList = binary_to_list(Hash),
    % Convert each byte to a hexadecimal representation
    HexHashList = lists:flatten([integer_to_list(Byte, 16) || Byte <- HashList]),
    % Join the list into a single string
    HexHashString = lists:flatten(HexHashList),
    HexHashString.

byte_sizee(Term) ->
    X = term_to_binary(Term),
    Y = byte_size(X),
    Y.

transaction_stake() ->
    Fun = fun() ->
        TransactionStake = #transaction_stake{
            cert_index = "",
            address = "",
            registration = ""},
        mnesia:write(TransactionStake),
        io:fwrite("~p~n", [TransactionStake]) 
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

transaction_delegation() ->
    Fun = fun() ->
        TransactinDelegation = #transaction_delegation{
            cert_index = "",
            address = "",
            pool_id = "",
            active_epoch = ""},
        mnesia:write(TransactinDelegation),
        io:fwrite("~p~n", [TransactinDelegation])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

transaction_withdrawal() ->
    Fun = fun() ->
        TransactinWithdrawal = #transaction_withdrawal{
            address = "",
            amount = ""},
        mnesia:write(TransactinWithdrawal),
        io:fwrite("~p~n", [TransactinWithdrawal])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

transaction_mir() ->
    Fun = fun() ->
        TransactinMir = #transaction_mir{
            pot = "",
            cert_index = "",
            address = "",
            amount = ""},
        mnesia:write(TransactinMir),
        io:fwrite("~p~n", [TransactinMir])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

transaction_pool_update() ->
    Fun = fun() ->
        TransactinPoolUpdate = #transaction_pool_update{
            cert_index = "",
            pool_id = "",
            vrf_key = "",
            pledge = "",
            margin_cost = "",
            fixed_cost = "",
            reward_account = "",
            owners = "",
            metadata = "",
            relays = "",
            active_epoch = ""},
        mnesia:write(TransactinPoolUpdate),
        io:fwrite("~p~n", [TransactinPoolUpdate])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

transaction_pool_retire() ->
    Fun = fun() ->
        TransactinPoolRetire = #transaction_pool_retire{
            cert_index = "",
            pool_id = "",
            retiring_epoch = ""},
        mnesia:write(TransactinPoolRetire),
        io:fwrite("~p~n", [TransactinPoolRetire])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

transaction_redeemer() ->
    Fun = fun() ->
        TransactionRedeemer = #transaction_redeemer{
            tx_index = "",
            purpose = "",
            unit_mem = "",
            unit_steps = "",
            fee = ""},
        mnesia:write(TransactionRedeemer),
        io:fwrite("~p~n", [TransactionRedeemer])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

transaction_utxos() ->
    Fun = fun() ->
        TransactinUtxos = #transaction_utxos{
            hash = "",
            inputs = "",
            outputs = ""},
        mnesia:write(TransactinUtxos),
        io:fwrite("~p~n", [TransactinUtxos])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

utxo_input() ->
    Fun = fun() ->
        UtxoInput = #utxo_input{
            address = "",
            amount = "",
            tx_hash = "",
            output_index = "",
            data_hash = "",
            collateral = ""},
        mnesia:write(UtxoInput),
        io:fwrite("~p~n", [UtxoInput])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

utxo_output() ->
    Fun = fun() ->
        UtxoOutput = #utxo_output{
            address = "",
            amount = "",
            data_hash = "",
            output_index = ""},
        mnesia:write(UtxoOutput),
        io:fwrite("~p~n", [UtxoOutput])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

utxos() ->
    Fun = fun() ->
        TransactionUtxos = #transaction_utxos{
            hash = "",
            inputs = utxo_input(),
            outputs = utxo_output()},
        mnesia:write(TransactionUtxos),
        io:fwrite("~p~n", [TransactionUtxos])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

