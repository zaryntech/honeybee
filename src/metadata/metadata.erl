-module(metadata).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([metadata_txs_label/0, metadata_txs_label_json/0, metadata_txs_label_cbor/0]).

metadata_txs_label() ->
    Fun = fun() ->
        MetadataTxsLabel = #metadata_txs_label{
            label = "",
            cip10 = "",
            count = ""},
        mnesia:write(MetadataTxsLabel),
        io:fwrite(MetadataTxsLabel)
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

metadata_txs_label_json() ->
    Fun = fun() ->
        MetadataTxsLabelJson = #metadata_txs_label_json{
            tx_hash = "",
            json_metadata = ""},
        mnesia:write(MetadataTxsLabelJson),
        io:fwrite("~p~n", [MetadataTxsLabelJson])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

metadata_txs_label_cbor() ->
    Fun = fun() ->
        MetadataTxsLabelCbor = #metadata_txs_label_cbor{
            tx_hash = "",
            cbor_metadata = ""},
        mnesia:write(MetadataTxsLabelCbor),
        io:fwrite("~p~n", [MetadataTxsLabelCbor])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.