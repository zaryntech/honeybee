-module(bech32_id).
-export([generate/1, generate_ids/2]).

% Function to generate a random ID of a specified length
generate(Length) when is_integer(Length), Length > 0 ->
    generate(Length, []).

generate(0, Acc) ->
    lists:reverse(Acc);
generate(Length, Acc) ->
    RandomChar = random_char(),
    generate(Length - 1, [RandomChar | Acc]).

% Function to generate a random alphanumeric character
random_char() ->
    AlphanumericChars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
    RandomIndex = random:uniform(length(AlphanumericChars)),
    lists:nth(RandomIndex, AlphanumericChars).

% Function to generate multiple random IDs
generate_ids(Count, Length) when is_integer(Count), is_integer(Length), Count > 0, Length > 0 ->
    generate_ids(Count, Length, []).

generate_ids(0, _Length, Acc) ->
    lists:reverse(Acc);
generate_ids(Count, Length, Acc) ->
    RandomID = generate(Length),
    generate_ids(Count - 1, Length, [RandomID | Acc]).
