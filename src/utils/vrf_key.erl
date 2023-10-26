-module(vrf_key).
-export([random/0]).

% Generate a random 65-character VRF key
random() ->
    BinaryKey = crypto:strong_rand_bytes(32), % 32 bytes for 256 bits
    VRFKey = base64:encode_to_string(BinaryKey),
    VRFKey.





