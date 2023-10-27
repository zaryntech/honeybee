-module(honeybee_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-include("records.hrl").
-define(SERVER, ?MODULE).

start_link() ->
    % create schema and directory
    application:set_env(mnesia, dir, "Mnesia/"),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:change_table_copy_type(schema, node(), disc_copies),

    %% enable httpc
    ssl:start(),
    application:start(inets),
    mnesia:create_table(account, [{attributes, record_info(fields, account)},
                                  {disc_copies, [node()]},
                                  {type, ordered_set}]),

    mnesia:create_table(account_reward, [{attributes, record_info(fields, account_reward)},
                                         {disc_copies, [node()]},
                                         {type, ordered_set}]),

    mnesia:create_table(account_history, [{attributes, record_info(fields, account_history)},
                                          {disc_copies, [node()]},
                                          {type, ordered_set}]),

    mnesia:create_table(account_delegation, [{attributes, record_info(fields, account_delegation)},
                                             {disc_copies, [node()]},
                                             {type, ordered_set}]),

    mnesia:create_table(account_registration, [{attributes, record_info(fields, account_registration)},
                                          {disc_copies, [node()]},
                                          {type, ordered_set}]),


    mnesia:create_table(address, [{attributes, record_info(fields, address)},
                                  {disc_copies, [node()]},
                                  {type, ordered_set}]),

    mnesia:create_table(address_total, [{attributes, record_info(fields, address_total)},
                                  {disc_copies, [node()]},
                                  {type, ordered_set}]),

    mnesia:create_table(address_utxo, [{attributes, record_info(fields, address_utxo)},
                                  {disc_copies, [node()]},
                                  {type, ordered_set}]),

    mnesia:create_table(address_transaction, [{attributes, record_info(fields, address_transaction)},
                                  {disc_copies, [node()]},
                                  {type, ordered_set}]),

    mnesia:create_table(block, [{attributes, record_info(fields, block)},
                                  {disc_copies, [node()]},
                                  {type, ordered_set}]),

    mnesia:create_table(genesis, [{attributes, record_info(fields, genesis)},
                                  {disc_copies, [node()]},
                                  {type, ordered_set}]),

    mnesia:create_table(network, [{attributes, record_info(fields, network)},
                                  {disc_copies, [node()]},
                                  {type, ordered_set}]),

    mnesia:create_table(network_supply, [{attributes, record_info(fields, network_supply)},
                                  {disc_copies, [node()]},
                                  {type, ordered_set}]),

    mnesia:create_table(network_stake, [{attributes, record_info(fields, network_stake)},
                                  {disc_copies, [node()]},
                                  {type, ordered_set}]),

    mnesia:create_table(pool, [{attributes, record_info(fields, pool)},
                               {disc_copies, [node()]},
                               {type, ordered_set}]),

    mnesia:create_table(pool_history, [{attributes, record_info(fields, pool_history)},
                                       {disc_copies, [node()]},
                                       {type, ordered_set}]),

    mnesia:create_table(pool_metadata, [{attributes, record_info(fields, pool_metadata)},
                                        {disc_copies, [node()]},
                                        {type, ordered_set}]),

    mnesia:create_table(pool_relay, [{attributes, record_info(fields, pool_relay)},
                                     {disc_copies, [node()]},
                                     {type, ordered_set}]),

    mnesia:create_table(pool_delegator, [{attributes, record_info(fields, pool_delegator)},
                                         {disc_copies, [node()]},
                                         {type, ordered_set}]),

    mnesia:create_table(pool_update, [{attributes, record_info(fields, pool_update)},
                                         {disc_copies, [node()]},
                                         {type, ordered_set}]),

    mnesia:create_table(transaction, [{attributes, record_info(fields, transaction)},
                                  {disc_copies, [node()]},
                                  {type, ordered_set}]),

    mnesia:create_table(utxo_input, [{attributes, record_info(fields, utxo_input)},
                                     {disc_copies, [node()]},
                                     {type, ordered_set}]),

    mnesia:create_table(utxo_output, [{attributes, record_info(fields, utxo_output)},
                                  {disc_copies, [node()]},
                                  {type, ordered_set}]),

    mnesia:create_table(transaction_utxos, [{attributes, record_info(fields, transaction_utxos)},
                                  {disc_copies, [node()]},
                                  {type, ordered_set}]),

    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

