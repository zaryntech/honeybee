-module(block).
-include("../records.hrl").
-author("Zaryn Technologies").
-export([genesis/0, new/0, calculateHash/1, byte_sizee/1, get_previous_block/0,
 get_block_by_id/1, get_all/0, get_latest_block_id/0, get_latest_blocks/0, get_block_time/1,
calculate_epoch_and_slot/1, calculate_height/0]).

genesis() ->
    Fun = fun() ->
        ID = nanoid:gen(),
        UnixTime = unix_time:now(),
        Block = #block{
            id = ID,
            time = UnixTime,
            height = 0,
            size = 0},
        mnesia:write(Block),
        Hash = calculateHash(Block),
        NewBlock = Block#block{hash = Hash},
        mnesia:write(NewBlock),
        io:fwrite("~p~n", [NewBlock]),
        Hash 
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.
% New Block
new() ->
    Fun = fun() ->
        ID = nanoid:gen(),
        UnixTime = unix_time:now(),
        {Epoch, Slot} = calculate_epoch_and_slot(UnixTime), % Calculate epoch and slot
        Bech32 = bech32_id:generate(50),
        VRFKey = vrf_key:random(),
        PrevBlock = get_previous_block(),
        Height = calculate_height(),
        Block = #block{
            id = ID,
            time = UnixTime,
            height = Height,
            slot = Slot,
            epoch = Epoch,
            epoch_slot = Slot rem 432000, % Slot within the epoch
            slot_leader = Bech32,
            tx_count = 0,
            output = 0,
            fees = "",
            block_vrf = VRFKey,
            previous_block = PrevBlock,
            next_block = "",
            confirmations = 0},
        mnesia:write(Block),
        Hash = calculateHash(Block),
        Size = byte_sizee(Block),
        NewBlock = Block#block{hash = Hash, size = Size},
        mnesia:write(NewBlock),
        io:fwrite("New Block Inserted ~p~n", [NewBlock]),
        Hash
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

% Calculate the Hash of Block
calculateHash(Block) ->
    Hash = crypto:hash(sha3_512, term_to_binary(Block)),
    HashList = binary_to_list(Hash),
    % Convert each byte to a hexadecimal representation
    HexHashList = lists:flatten([integer_to_list(Byte, 16) || Byte <- HashList]),
    % Join the list into a single string
    HexHashString = lists:flatten(HexHashList),
    HexHashString.

byte_sizee(Term) ->
    X = term_to_binary(Term),
    Y = byte_size(X),
    Y.

get_previous_block() ->
    LatestBlockID = get_latest_block_id(),
    Block = get_block_by_id(LatestBlockID),
    Block.
% Get the Block Info by BlockID
get_block_by_id(ID) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#block{id = ID, _ = '_'})
            end),
    case Res of
        {atomic, []} -> block_not_exist;
        {atomic, [Block]} -> Block;
        _ -> error
    end. 

% Get all the BlockIDs as a list   
get_all() ->
    Fun = fun() ->
            mnesia:dirty_all_keys(block)
            end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

% Get the ID of latest Block in the Network
get_latest_block_id() ->
    BlockIds = block:get_latest_blocks(),
    case lists:reverse(BlockIds) of
        [LatestId | _Rest] -> LatestId;  % Get the first element of the reversed list
        [] -> undefined  % No blocks found
    end.

% Get Latest BlockID based on Timestamp
get_latest_blocks() ->
    Fun = fun() ->
        BlockIDs = block:get_all(),
        % Sort the Block IDs by timestamp in ascending order
        SortedBlockIDs = lists:sort(fun(ID1, ID2) -> block:get_block_time(ID1) < block:get_block_time(ID2) end, BlockIDs)
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res. 

% Get the Block Time by BlockID
get_block_time(ID) ->
    Res = mnesia:transaction(
            fun() ->
                mnesia:match_object(#block{id = ID, _ = '_'})
            end),
    case Res of
        {atomic, []} -> block_not_exist;
        {atomic, [Block]} -> Block#block.time;
        _ -> error
    end. 

% Calculate Epoch and Slot based on UNIX time
calculate_epoch_and_slot(UnixTime) ->
    {Epoch, Slot} = calculate_epoch_and_slot_recursive(UnixTime, 0, 0),
    {Epoch, Slot}.

calculate_epoch_and_slot_recursive(UnixTime, CurrentEpoch, CurrentSlot) when CurrentSlot >= 432000 ->
    % If the current slot exceeds the maximum slots per epoch, move to the next epoch
    calculate_epoch_and_slot_recursive(UnixTime, CurrentEpoch + 1, 0);

calculate_epoch_and_slot_recursive(UnixTime, CurrentEpoch, CurrentSlot) ->
    % Calculate the current epoch and slot
    SlotDuration = 1, % One second per slot
    SlotsPerEpoch = 432000, % 5 days (432000 seconds) per epoch
    Epoch = CurrentEpoch + (UnixTime div (SlotDuration * SlotsPerEpoch)),
    Slot = (UnixTime rem (SlotDuration * SlotsPerEpoch)) div SlotDuration,
    {Epoch, Slot}.

calculate_height() ->
    Block = get_previous_block(),
    Height = Block#block.height,
    New = Height + 1,
    New.

    
    


    
