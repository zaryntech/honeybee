-module(find_dns).
-export([find/0]).

find() ->
    {ok, Hostname} = inet:gethostname(),
    B = inet_res:getbyname(Hostname, aaaa),
    B.
    
    

