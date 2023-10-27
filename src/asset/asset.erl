-module(asset).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([insert/0, asset_address/0, asset_history/0, asset_transaction/0, asset_metadata/0, 
asset_onchain_metadata/0]).

insert() ->
    Fun = fun() ->
        AssetDetails = #asset_details{
            asset = "",
            policy_id = "",
            asset_name = "",
            fingerprint = "",
            quantity = "",
            initial_mint_tx_hash = "",
            mint_or_burn_count = "",
            onchain_metadata = asset_onchain_metadata(),
            metadata = asset_metadata()},
        mnesia:write(AssetDetails),
        io:fwrite("~p~n", [AssetDetails])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

asset_address() ->
    Fun = fun() ->
        AssetAddress = #asset_address{
            address = "",
            quantity = ""},
        mnesia:write(AssetAddress),
        io:fwrite("~p~n", [AssetAddress])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

asset_history() ->
    Fun = fun() ->
        AssetHistory = #asset_history{
            tx_hash = "",
            amount = "",
            action = ""},
        mnesia:write(AssetHistory),
        io:fwrite("~p~n", [AssetHistory])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

asset_transaction() ->
    Fun = fun() ->
        AssetTransaction = #asset_transaction{
            tx_hash = "",
            tx_index = "",
            block_height = "",
            block_time = ""},
        mnesia:write(AssetTransaction),
        io:fwrite("~p~n", [AssetTransaction])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

asset_metadata() -> 
    Fun = fun() ->
        AssetMetadata = #asset_metadata{
            name = "",
            description = "",
            ticker = "",
            url = "",
            logo = "",
            decimals = ""},
        mnesia:write(AssetMetadata),
        io:fwrite("~p~n", [AssetMetadata])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Fun.

asset_onchain_metadata() -> 
    Fun = fun() ->
        AssetOnchainMetadata = #asset_onchain_metadata{
            name = "",
            image = ""},
        mnesia:write(AssetOnchainMetadata),
        io:fwrite("~p~n", [AssetOnchainMetadata])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

