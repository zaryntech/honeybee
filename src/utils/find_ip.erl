-module(find_ip).
-export([all/0]).

all() ->
    inet:getifaddrs().
