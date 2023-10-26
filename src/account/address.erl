-module(address).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([insert/0, address_total/0, address_utxo/0, address_transaction/0]).

insert() ->
    Fun = fun() ->
        RandAddress = random_address:generate(40),
        Bech32Address = bech32_id:generate(22),
        Address = #address{
            address = RandAddress,
            amount = 0,
            stake_address = Bech32Address,
            type = undefined},
        mnesia:write(Address),
        io:fwrite("Address Generated: ~p~n", [Address])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

address_total() ->
    Fun = fun() ->
        Bech32Address = bech32_id:generate(30),
        AddressTotal = #address_total{
            address = Bech32Address,
            received_sum = 0,
            sent_sum = 0,
            tx_count = 0},
        mnesia:write(AddressTotal),
        io:fwrite("~p~n", [AddressTotal])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

address_utxo() ->
    Fun = fun() ->
        Bech32Address = bech32_id:generate(28),
        AddressUtxo = #address_utxo{
            address = Bech32Address,
            tx_hash = "",
            output_index = 0,
            amount = 0,
            block = 0,
            data_hash = 0,
            inline_datum = 0,
            reference_script_hash= 0},
        mnesia:write(AddressUtxo),
        io:fwrite("~p~n", [AddressUtxo])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

address_transaction() ->
    Fun = fun() ->
        AddressTransaction = #address_transaction{
            tx_hash = "",
            tx_index = 0,
            block_height = 0},
        mnesia:write(AddressTransaction),
        io:fwrite("~p~n", [AddressTransaction])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.