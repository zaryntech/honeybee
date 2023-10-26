-module(transaction).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([insert/0, calculateHash/1, byte_sizee/1]).

insert() ->
    Fun = fun() ->
        Transaction = #transaction{
            hash = calculateHash(#transaction{}),
            block = 0,
            block_height = 0,
            block_time = 0,
            slot = 0,
            index = 0,
            output_amount = 0,
            fees = "",
            deposit = "",
            size = byte_sizee(#transaction{}),
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
        io:fwrite(" Transaction Inserted ~p~n", [Transaction])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

calculateHash(Data) ->
    crypto:hash(sha256, term_to_binary(Data)).

byte_sizee(Term) ->
    X = term_to_binary(Term),
    Y = byte_size(X),
    Y.