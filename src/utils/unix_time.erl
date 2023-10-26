-module(unix_time).
-author("Zaryn Technologies").
-export([now/0]).

now() ->

    % Get the current time in microseconds since the epoch
    MicrosecondsSinceEpoch = os:system_time(),
    % Convert to seconds (divide by 1,000,000)
    UnixTimestamp = MicrosecondsSinceEpoch div 1000000,
    UnixTimestamp.
