-module(transaction).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([insert/0, calculateHash/1, byte_sizee/1, utxo_input/0, utxo_output/0]).

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

