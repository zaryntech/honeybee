-module(address_api).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([get_address/1, get_total/1, get_utxo/1, get_transaction/1]).

% Get the Address Info by Address
get_address(Address) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#address{address = Address, _ = '_'})
            end),
    case Res of
        {atomic, []} -> address_not_exist;
        {atomic, [AddressInfo]} -> AddressInfo;
        _ -> error
    end. 

get_total(Address) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#address_total{address = Address, _ = '_'})
            end),
    case Res of
        {atomic, []} -> address_not_exist;
        {atomic, [AddressTotal]} -> AddressTotal;
        _ -> error
    end. 

get_utxo(Address) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#address_utxo{address = Address, _ = '_'})
            end),
    case Res of
        {atomic, []} -> address_not_exist;
        {atomic, [AddressUtxo]} -> AddressUtxo;
        _ -> error
    end. 

get_transaction(Address) ->
    'not implemented'.