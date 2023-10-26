-module(genesis).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([insert/0]).

insert() ->
    Fun = fun() ->
        Timestamp = calendar:universal_time(),
        Genesis = #genesis{
            active_slots_coefficient = 0.05,
            update_quorum = 5,
            max_lovelace_supply = 45000000000000000,
            network_magic = 764824073,
            epoch_lenght = 432000,
            system_start = Timestamp,
            slots_per_kes_period = 129600,
            slot_lenght = 1,
            max_kes_evolutions = 62,
            security_param = 2160},
        mnesia:write(Genesis),
        io:fwrite("~p~n", [Genesis])
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.