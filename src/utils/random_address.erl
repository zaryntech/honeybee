-module(random_address).
-export([generate/1, generate/2]).

generate(Length) ->
    CharSet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
    generate(Length, CharSet).

generate(Length, CharSet) when is_integer(Length), Length > 0, is_list(CharSet) ->
    RandomString = generate_random_string(Length, CharSet),
    RandomString.

generate_random_string(0, _) ->
    [];
generate_random_string(Length, CharSet) ->
    RandomChar = lists:nth(rand:uniform(length(CharSet)) + 1, CharSet),
    [RandomChar | generate_random_string(Length - 1, CharSet)].
