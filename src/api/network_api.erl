-module(network_api).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([get_network/1]).

get_network(ID) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#network{id = ID, _ = '_'})
            end),
    case Res of
        {atomic, []} -> network_not_exist;
        {atomic, [Network]} -> Network;
        _ -> error
    end.