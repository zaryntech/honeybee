-record(account, {
    stake_address, % Bech32 stake address
    active, % Balance of the account in Lovelaces.
    active_epoch, % Epoch of the most recent action
    controlled_amount, % Balance of the account in Lovelaces
    rewards_sum, % Sum of all funds rewards for the account
    withdrawals_sum, % Sum of all the withdrawals for the account
    reserves_sum, % Sum of all funds from reserves for the account
    treasury_sum, % Sum of all funds from treasury for the account
    withdrawable_amount, % Sum of available rewards that haven't been withdrawn yet 
    pool_id
}). 
-record(account_reward, {
    epoch, % Epoch of the associated reward.
    amount, % Rewards for given epoch in Lovelaces.
    pool_id, % Bech32 pool ID being delegated to.
    type % RewardType -> Leader | Member | PoolDepositRefund
}).
-record(account_history, {
    active_epoch, % Epoch of the associated reward.
    amount, % Stake amount in Lovelaces
    pool_id  % Bech32 pool ID being delegated to.
}).
-record(account_delegation, {
    active_epoch, % Epoch in which the delegation becomes active.
    tx_hash, % Hash of the transaction containing the delegation.
    amount, % Rewards for given epoch in Lovelaces.
    pool_id % Bech32 ID of pool being delegated to.
}).
-record(account_registration, {
    tx_hash, %  Hash of the transaction containing the (de)registration certificate.
    action % Action in the certificate. Registration action -> Registered | Deregistered
}).
-record(account_withdrawal, {
    tx_hash, % Hash of the transaction containing the withdrawal.
    amount % Withdrawal amount in Lovelaces.
}).
-record(account_mir, {
    tx_hash, % Hash of the transaction containing the MIR.
    amount % MIR amount in Lovelaces.
}).

-record(account_address, {
    address % Address associated with the stake key.
}).
-record(account_address_asset, {
    unit, % Format: Concatenation of asset `policy_id` and hex-encoded `asset_name`.
    quantity % The quantity of the unit.
}).
-record(address, { 
    address, % Bech32 encoded addresses.
    amount, % Sum of all owned assets.
    stake_address, % Stake address that controls the key.
    type % Address era
}).
-record(address_total, { 
    address, % Bech32 encoded address
    received_sum, % Sum of all transaction received assets.
    sent_sum, % Sum of all transaction sent assets.
    tx_count % Count of all transactions on the address.
}).
-record(address_utxo, {
    address, % Bech32 encoded addresses - useful when querying by payment_cred
    tx_hash, % Transaction hash of the UTXO.
    output_index, % UTXO index in the transaction.
    amount, % Sum of assets for this UTXO.
    block, % Block hash of the UTXO.
    data_hash, % The hash of the transaction output datum
    inline_datum, % CBOR encoded inline datum
    reference_script_hash % The hash of the reference script of the output
}).
-record(address_transaction, {
    tx_hash, % Hash of the transaction.
    tx_index, % Transaction index within the block.
    block_height, % Block height.
    block_time % Block Time (Unix time)
}).
-record(address_stake, {
    stake_address, % Stake address.
    amount % Amount of active delegated stake in Lovelaces.
}).
-record(address_stake_pool, {
    stake_address, % Stake address.
    pool_id, % Bech32 prefix of the pool delegated to.
    amount % Amount of active delegated stake in Lovelaces.
}).
-record(asset, {
    asset, % Format: Concatenation of asset `policy_id` and hex-encoded `asset_name`.
    quantity % Current asset quantity
}).
-record(asset_details, {
    asset, % Hex-encoded asset full name
    policy_id, % Policy ID of the asset
    asset_name, % Hex-encoded asset name of the asset
    fingerprint, % CIP14 based user-facing fingerprint
    quantity, % Current asset quantity
    initial_mint_tx_hash, % ID of the initial minting transaction
    mint_or_burn_count, % Count of mint and burn transactions
    onchain_metadata, % On-chain metadata stored in the minting transaction under label 721
    metadata
}).
-record(asset_address, {
    address, % Address containing the specific asset
    quantity % Asset quantity on the specific address
}).
-record(asset_history, {
    tx_hash, % Hash of the transaction containing the asset action
    amount, % Asset amount of the specific action
    action % Action executed upon the asset policy
}).
-record(asset_metadata, {
    name, % Asset name
    description, % Asset description
    ticker,
    url, % Asset website
    logo, % Base64 encoded logo of the asset
    decimals % Number of decimal places of the asset unit
}).
-record(asset_onchain_metadata, {
    name, % Name of the asset
    image % URI of the associated asset
}).
-record(asset_policy, {
    asset,
    quantity
}).
-record(asset_transaction, {
    tx_hash, % Hash of the transaction
    tx_index, % Transaction index within the block
    block_height, % Block height
    block_time % Block creation time in UNIX time
}).
-record(block, {
    id,
    time, % Block creation time in UNIX time.
    height, % Block number.
    hash, % Hash of the block
    slot, % Slot Number
    epoch, % Epoch Number
    epoch_slot, % Slot within the epoch
    slot_leader, % Bech32 ID of the slot leader or specific block description in case there is no slot leader.
    size, % Block size in Bytes.
    tx_count, %  Number of transactions in the block.
    output, % Total output within the block 
    fees, % Total fees within the block in Lovelaces.
    block_vrf, % VRF key of the block (exactly 65 characters).
    previous_block, % Hash of the previous block.
    next_block, % Hash of the next block.
    confirmations % Number of block confirmations.
}).
-record(epoch, {
    epoch, % Epoch number.
    start_time, % Unix time of the start of the epoch.
    end_time, % Unix time of the end of the epoch.
    first_block_time, % Unix time of the first block of the epoch.
    last_block_time, % Unix time of the last block of the epoch.
    block_count, % Number of blocks within the epoch.
    tx_count, % Number of transactions within the epoch.
    output, % Sum of all the transactions within the epoch in Lovelaces.
    fees, % Sum of all the fees within the epoch in Lovelaces.
    active_stake % Sum of all the active stakes within the epoch in Lovelaces.
}).
-record(epoch_parameters, {
    epoch, % Epoch number
    min_fee_a, % The linear factor for the minimum fee calculation for given epoch
    min_fee_b, % The constant factor for the minimum fee calculation
    max_block_size, % Maximum block body size in Bytes
    max_tx_size, % Maximum transaction size
    max_block_header_size, % Maximum block header size
    key_deposit, % The amount of a key registration deposit in Lovelaces
    pool_deposit, % The amount of a pool registration deposit in Lovelaces
    e_max, % Epoch bound on pool retirement
    n_opt, % Desired number of pools
    a0, % Pool pledge influence
    rho, % Monetary expansion
    tau, % Treasury expansion
    decentralisation_param, % Percentage of blocks produced by federated nodes
    extra_entropy, % Seed for extra entropy
    protocol_major_ver, % Accepted protocol major version
    protocol_minor_ver, % Accepted protocol minor version
    min_utxo, % Minimum UTXO value
    min_pool_cost, % Minimum stake cost forced on the pool
    nonce, % Epoch number only used once
    cost_models, % Cost models parameters for Plutus Core scripts
    price_mem, % The per word cost of script memory usage
    price_step, % The cost of script execution step usage
    max_tx_ex_mem, % The maximum number of execution memory allowed to be used in a single transaction
    max_tx_ex_steps, % he maximum number of execution steps allowed to be used in a single transaction
    max_block_ex_mem, % The maximum number of execution memory allowed to be used in a single block
    max_block_ex_steps, % The maximum number of execution steps allowed to be used in a single block
    max_val_size, % The maximum Val size
    collateral_percent, % The percentage of the transactions fee which must be provided as collateral when including non-native scripts
    max_collateral_inputs, % The maximum number of collateral inputs allowed in a transaction
    coins_per_utxo_size % The cost per UTxO size. Cost per UTxO *word* for Alozno. Cost per UTxO *byte* for Babbage and later
}).
-record(epoch_stake, {
    stake_address, % Stake address
    pool_id, % Bech32 prefix of the pool delegated to
    amount % Amount of active delegated stake in Lovelaces
}).
-record(epoch_stake_pool, {
    stake_address,
    amount
}).
-record(genesis, {
    active_slots_coefficient, % The proportion of slots in which blocks should be issued
    update_quorum, % Determines the quorum needed for votes on the protocol parameter updates
    max_lovelace_supply, % The total number of lovelace in the system
    network_magic, % Network identifier
    epoch_lenght, % Number of slots in an epoch
    system_start, % Time of slot 0 in UNIX time
    slots_per_kes_period, % Number of slots in an KES period
    slot_lenght, % Duration of one slot in seconds
    max_kes_evolutions, % The maximum number of time a KES key can be evolved before a pool operator must create a new operational certificate
    security_param % Security parameter @k@
}).
-record(validator, {
    id,
    peer,
    wallet,
    fika 
}).
-record(peer, {
    id, 
    pid,
    host 
}).
-record(wallet, {
    id,
    validator,
    address,
    fika,
    token = [],
    keypair,
    pubKey,
    privKey,
    balance,
    transaction = []
}).
-record(metadata_txs_label, {
    label, % Metadata label.
    cip10, % CIP10 defined description.
    count % The count of metadata entries with a specific label.
}).
-record(metadata_txs_label_json, {
    tx_hash, % Transaction hash that contains the specific metadata.
    json_metadata % Content of the JSON metadata.
}).
-record(metadata_txs_label_cbor, {
    tx_hash, % Transaction hash that contains the specific metadata.
    cbor_metadata % Content of the CBOR metadata.
}).
-record(script, {
    id,
    script_hash, % Hash of the script
    type, % Type of the script language -> PlutusV1 | PlutusV2 | Timelock
    serialised_size % The size of the CBOR serialised script, if a Plutus script
}).
-record(script_redeemer, {
    id,
    tx_hash, % Hash of the transaction
    tx_index, % Index of the redeemer within a transaction
    purpose, % Validation purpose
    redeemer_data_hash, % Datum hash of the redeemer
    unit_mem, % The budget in Memory to run a script
    unit_steps, % The budget in Steps to run a script
    fee % The fee consumed to run the script
}).
-record(transaction, {
    id, % Transactin ID
    hash, % Transaction hash
    block, % Block hash
    block_height, % Block number
    block_time, % Block creation time
    slot, % Slot number
    index, % Transaction index within the block
    output_amount, % Amounts of the transaction.
    fees, % Fees of the transaction
    deposit, % Deposit within the transaction
    size, % Size of the transaction in Bytes.
    invalid_before, % Left (included) endpoint of the timelock validity intervals.
    invalid_hereafter, % Right (excluded) endpoint of the timelock validity intervals.
    utxo_count, % Count of UTXOs within the transaction.
    withdrawal_count, % Count of the withdrawals within the transaction.
    mir_cert_count, % Count of the MIR certificates within the transaction.
    delegation_count, % Count of the delegations within the transaction.
    stake_cert_count, % Count of the stake keys (de)registration and delegation certificates within the transaction.
    pool_update_count, % Count of the stake pool registration and update certificates within the transaction.
    pool_retire_count, % Count of the stake pool retirement certificates within the transaction.
    asset_mint_or_burn_count, % Count of asset mints and burns within the transaction.
    redeemer_count % Count of redeemers within the transaction.
}).
-record(transaction_utxos, {
    hash, % Transaction hash
    inputs, % Transaction inputs
    outputs % Transaction outputs
}).
-record(transaction_stake, { 
    cert_index, % Index of the certificate within the transaction
    address, % Delegation stake address
    registration % Registration boolean, false if deregistration
}).
-record(transaction_delegation, {
    cert_index, % Index of the certificate within the transaction
    address, % Delegation stake address
    pool_id, % Bech32 ID of delegated stake pool
    active_epoch % Epoch in which the delegation becomes active
}).
-record(transaction_withdrawal, {
    address, % Bech32 withdrawal address
    amount % Withdrawal amount in Lovelaces
}).
-record(transaction_mir, {
    pot, % Source of MIR funds
    cert_index, % Index of the certificate within the transaction
    address, % Bech32 stake address
    amount % MIR amount in Lovelaces
}).
-record(transaction_pool_update, {
    cert_index, % Index of the certificate within the transaction
    pool_id, % Bech32 encoded pool ID
    vrf_key, % VRF key hash
    pledge, % Stake pool certificate pledge in Lovelaces
    margin_cost, % Margin tax cost of the stake pool
    fixed_cost, % Fixed tax cost of the stake pool in Lovelaces
    reward_account, % Bech32 reward account of the stake pool
    owners,
    metadata,
    relays, % [PoolRelay]
    active_epoch % Epoch that the delegation becomes active
}).
-record(transaction_pool_retire, {
    cert_index, % Index of the certificate within the transaction
    pool_id, % Bech32 stake pool ID
    retiring_epoch % Retiring epoch
}).
-record(transaction_metadata, {
    label, % Metadata label
    json_metadata % Content of the JSON metadata
}).
-record(transaction_metadata_cbor, {
    label, % Metadata label
    cbor_metadata % Content of the CBOR metadata
}).
-record(transaction_redeemer, {
    tx_index, % Index of the redeemer within a transaction
    purpose, % Validation purpose
    unit_mem, % The budget in Memory to run a script
    unit_steps, % The budget in Steps to run a script
    fee % The fee consumed to run the script
}).
-record(utxo_input, {
    address, % Input address
    amount, 
    tx_hash, % Hash of the UTXO transaction
    output_index, % UTXO index in the transaction
    data_hash, % The hash of the transaction output datum
    collateral % UTXO is a script collateral input
}).
-record(utxo_output, {
    address, % Output address
    amount, % Transaction output amounts
    data_hash, % The hash of the transaction output datum
    output_index, % UTXO index in the transaction
    collateral % UTXO is a script collateral output
}).
-record(pool_update_metadata, {
    url, % URL to the stake pool metadata
    hash, % Hash of the metadata file
    ticker, % Ticker of the stake pool
    name, % Name of the stake pool
    description, % Description of the stake pool
    homepage % Home page of the stake pool
}).
-record(relay, {
    ipv4,
    ipv6,
    dns,
    dns_srv,
    port 
}).
-record(ledger, {
    id 
}).
-record(pool, {
    pool_id, % Bech32 encoded pool ID
    hex, % Hexadecimal pool ID
    vrf_key, % VRF key hash
    blocks_minted, % Total minted blocks
    live_stake,
    live_size,
    live_saturation,
    live_delegators,
    active_stake, 
    active_size,
    declared_pledge, % Stake pool certificate pledge
    live_pledge, % Stake pool current pledge.
    margin_cost, % Margin tax cost of the stake pool.
    fixed_cost, % Fixed tax cost of the stake pool.
    reward_account, % Bech32 reward account of the stake pool.
    owners, 
    registration,
    retirement
}).
-record(pool_history, {
    epoch, % Epoch number
    blocks, % Number of blocks created by pool
    active_stake, % Active (Snapshot of live stake 2 epochs ago) stake in Lovelaces.
    active_size, % Pool size (percentage) of overall active stake at that epoch.
    delegators_count, % Number of delegators for epoch.
    rewards, % Total rewards received before distribution to delegators.
    fees % Pool operator rewards.
}).
-record(pool_metadata, {
    pool_id, % Bech32 pool ID
    hex, % Hexadecimal pool ID.
    url, % URL to the stake pool metadata.
    hash, % Hash of the metadata file.
    ticker, % Ticker of the stake pool
    name, % Name of the stake pool
    description, % Description of the stake pool
    homepage % Home page of the stake pool.
}).
-record(pool_relay, {
    ipv4, % IPv4 address of the relay.
    ipv6, % IPv6 address of the relay.
    dns, % DNS name of the relay.
    dns_srv, % DNS SRV entry of the relay.
    port % Network port of the relay.
}).
-record(pool_delegator, {
    address, % Bech32 encoded stake addresses.
    live_stake % Currently delegated amount.
}).
-record(pool_update, {
    tx_hash, % Transaction ID.
    cert_index, % Certificate within the transaction.
    action % Action in the certificate.
}).
-record(pool_retire, {
    pool_id, % Bech32 encoded pool ID
    epoch % Retirement epoch number
}).
-record(network, {
    id, % Network ID
    supply, % Current live stake in Lovelaces
    stake  % Current active stake in Lovelaces
}).
-record(network_supply, {
    id, % Network ID
    max, % Maximum supply in Lovelaces
    total, % Current total (max supply - reserves) supply in Lovelaces
    circulating, % Current circulating (UTXOs + withdrawables) supply in Lovelaces
    locked, % Current supply locked by scripts in Lovelaces
    treasury, % Current supply locked in treasury
    reserves % Current supply locked in reserves
}).
-record(network_stake, {
    id, % Network ID
    live, % Current live stake in Lovelaces
    active % Current active stake in Lovelaces
}).
-record(hb_node, {
    host,
    port
}).


