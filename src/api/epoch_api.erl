-module(epoch_api).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([get_epoch/1, get_latest_epoch/1, get_epoch_parameters/1, get_epoch_stake/1,
get_epoch_stake_pool/1]).

get_epoch(Epoch) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#epoch{epoch = Epoch, _ = '_'})
            end),
    case Res of
        {atomic, []} -> epoch_not_exist;
        {atomic, [Epoch]} -> Epoch;
        _ -> error
    end.

get_latest_epoch(Epoch) ->
    'not implemented'.

get_epoch_parameters(Epoch) ->
    'not implemented'.

get_epoch_stake(Epoch) ->
    'not implemented'.

get_epoch_stake_pool(Epoch) ->
    'not implemented'.