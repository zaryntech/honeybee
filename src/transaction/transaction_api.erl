-module(transaction_api).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([get_transaction_by_hash/1, get_utxos/1, get_radeemers/1, get_stakes/1,
get_delegations/1, get_withdrawals/1, get_mirs/1, get_pool_updates/1, get_pool_retire/1,
get_metadata/1, get_metadata_cbor/1]).

get_transaction_by_hash(Hash) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#transaction{hash = Hash, _ = '_'})
            end),
    case Res of
        {atomic, []} -> account_not_exist;
        {atomic, [Transaction]} -> Transaction;
        _ -> error
    end. 

get_utxos(Hash) ->
    'not implemented'.

get_radeemers(Hash) ->
    'not implemented'.

get_stakes(Hash) ->
    'not implemented'.

get_delegations(Hash) ->
    'not implemented'.

get_withdrawals(Hash) ->
    'not implemented'.

get_mirs(Hash) ->
    'not implemented'.

get_pool_updates(Hash) ->
    'not implemented'.

get_pool_retire(Hash) ->
    'not implemented'.

get_metadata(Hash) ->
    'not implemented'.

get_metadata_cbor(Hash) ->
    'not implemented'.