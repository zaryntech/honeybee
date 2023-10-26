-module(block_api).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([get_block/1, get_by_slot/1, get_by_epoch_slot/2, get_block_transactions/1,
get_latest_block/0, get_next_blocks/1]).

get_block(Hash) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#block{hash = Hash, _ = '_'})
            end),
    case Res of
        {atomic, []} -> block_not_exist;
        {atomic, [Block]} -> Block;
        _ -> error
    end.

get_by_slot(Slot) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#block{slot = Slot, _ = '_'})
            end),
    case Res of
        {atomic, []} -> block_not_exist;
        {atomic, [Block]} -> Block;
        _ -> error
    end.

get_by_epoch_slot(Epoch, Slot) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#block{epoch = Epoch, slot = Slot, _ = '_'})
            end),
    case Res of
        {atomic, []} -> block_not_exist;
        {atomic, [Block]} -> Block;
        _ -> error
    end.

%
get_block_transactions(Hash) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#block{hash = Hash, _ = '_'})
            end),
    case Res of
        {atomic, []} -> block_not_exist;
        {atomic, [Block]} -> Block#block.tx_count;
        _ -> error
    end.

get_latest_block() ->
    LatestBlock = block:get_previous_block(),
    LatestBlock.

get_next_blocks(Hash) ->
    'not implemented'.