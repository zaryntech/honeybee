-module(hex_id).
-export([generate/1]).

generate(Length) when is_integer(Length), Length > 0 ->
    RandomBytes = crypto:strong_rand_bytes(Length),
    HexString = lists:flatten([io_lib:format("~2.16.0B", [X]) || <<X:8>> <= RandomBytes]),
    HexString.
