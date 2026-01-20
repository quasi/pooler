# API Reference

Complete reference for all Pooler functions and macros.

## Basic Pool Operations

### `make-pool`

Creates a new pool for managing reusable objects.

```lisp
(make-pool &key name capacity threshold item-maker item-destroyer timeout)
```

**Parameters**:
- `name` (string, default: "Default Pool") - Identifying name for debugging
- `capacity` (integer, default: 40) - Maximum number of items pool can hold
- `threshold` (integer, default: 2) - Minimum items to keep ready
- `item-maker` (function) - Called to create new items. Takes no arguments.
- `item-destroyer` (function) - Called to clean up items. Takes item as argument.
- `timeout` (integer, default: 300) - Seconds before pool auto-refreshes when idle

**Returns**: A pool object

**Example**:
```lisp
(make-pool :name "DB Pool"
           :capacity 10
           :item-maker (lambda () (connect-to-db))
           :item-destroyer (lambda (conn) (disconnect conn)))
```

---

### `fetch-from`

Get an item from the pool.

```lisp
(fetch-from pool &key (tries 2))
```

**Parameters**:
- `pool` - The pool to fetch from
- `tries` (integer, default: 2) - Number of times to retry on empty pool

**Returns**: An item from the pool (or creates new if empty)

**Behavior**:
- If pool has items, returns one immediately
- If pool is empty, grows pool and returns new item
- If pool is old (timeout exceeded), refreshes pool first
- Retries up to `tries` times if creation fails

**Errors**:
- `pool-error` - Could not get item after `tries` attempts
- `pool-item-creation-error` - Item maker failed

**Example**:
```lisp
(let ((item (fetch-from pool)))
  (use-item item))
```

---

### `return-to`

Return an item to the pool.

```lisp
(return-to pool item)
```

**Parameters**:
- `pool` - The pool to return to
- `item` - The item to return

**Returns**: New current pool size

**Behavior**:
- If pool has room: item is enqueued and available for next fetch
- If pool is full: item is immediately destroyed (not stored)

**Example**:
```lisp
(return-to pool item)
```

---

### `with-pool`

Macro for safe fetch/return with automatic cleanup.

```lisp
(with-pool (variable pool) &body body)
```

**Parameters**:
- `variable` - Symbol to bind the fetched item to
- `pool` - The pool to fetch from
- `body` - Code to execute with the item

**Behavior**:
- Fetches item from pool
- Executes body with item bound to `variable`
- Automatically returns item, even if body signals error

**Example**:
```lisp
(with-pool (connection db-pool)
  (query connection "SELECT * FROM users"))
```

---

### `grow-pool`

Pre-create items in the pool.

```lisp
(grow-pool pool &optional grow-by)
```

**Parameters**:
- `pool` - The pool to grow
- `grow-by` (integer, default: pool's threshold) - Number of items to create

**Returns**: NIL

**Behavior**:
- Creates and enqueues `grow-by` items
- Won't exceed pool capacity
- Useful for warming up the pool on startup

**Example**:
```lisp
(grow-pool db-pool 5)  ; Create 5 connections upfront
```

---

### `pool-init`

Refresh the pool by destroying all items and recreating.

```lisp
(pool-init pool)
```

**Parameters**:
- `pool` - The pool to refresh

**Returns**: NIL

**Behavior**:
- Destroys all existing items (calls item-destroyer on each)
- Resets metrics (current-size, total-uses, total-created → 0)
- Increments total-pool-inits counter
- Signals `pool-warning` condition
- Grows pool back to threshold size

**When to use**:
- After timeout expires (automatic)
- If pool items become stale or invalid
- To force refresh of connections/resources

**Example**:
```lisp
(pool-init db-pool)  ; Refresh all connections
```

---

## Distributed Pool Operations

### `make-distributed-pool`

Create a distributed pool with multiple nodes.

```lisp
(make-distributed-pool &key name node-configs replicas item-maker
                             item-destroyer default-capacity
                             default-threshold default-timeout)
```

**Parameters**:
- `name` (string) - Identifying name
- `node-configs` (list of plists) - Node configurations
- `replicas` (integer, default: 10) - Virtual nodes per physical node
- `item-maker` (function) - Global item creation function (used if node doesn't specify)
- `item-destroyer` (function) - Global item cleanup function
- `default-capacity` (integer, default: 40) - Default capacity for nodes
- `default-threshold` (integer, default: 2) - Default threshold for nodes
- `default-timeout` (integer, default: 300) - Default timeout for nodes

**Node Config Parameters**:
Each element in `node-configs` is a plist with:
- `:name` (string, required) - Node identifier
- `:capacity` (integer) - Node's capacity (uses default if omitted)
- `:threshold` (integer) - Node's threshold (uses default if omitted)
- `:timeout` (integer) - Node's timeout (uses default if omitted)
- `:item-maker` (function) - Node-specific item creator (uses global if omitted)
- `:item-destroyer` (function) - Node-specific item destroyer (uses global if omitted)

**Returns**: A distributed pool object

**Example**:
```lisp
(make-distributed-pool
  :name "User Shards"
  :node-configs (list
    (list :name "shard-1")
    (list :name "shard-2")
    (list :name "shard-3"))
  :item-maker (lambda () (connect-to-db))
  :item-destroyer (lambda (conn) (disconnect conn)))
```

---

### `fetch-from-distributed-pool`

Get an item from the distributed pool based on a key.

```lisp
(fetch-from-distributed-pool dist-pool key &key (tries 2))
```

**Parameters**:
- `dist-pool` - The distributed pool
- `key` (string) - Key determining which node to fetch from
- `tries` (integer, default: 2) - Retry attempts

**Returns**: An item from the appropriate node

**Behavior**:
- Hashes key to find target node
- Fetches item from that node
- Same key always goes to same node (consistent hashing)

**Example**:
```lisp
(fetch-from-distributed-pool shard-pool "user:123")
```

---

### `return-to-distributed-pool`

Return an item to the distributed pool.

```lisp
(return-to-distributed-pool dist-pool key item)
```

**Parameters**:
- `dist-pool` - The distributed pool
- `key` (string) - Key used when fetching
- `item` - The item to return

**Returns**: New current pool size

**Behavior**:
- Routes to same node as fetch (based on key)
- Returns item using same rules as regular pools

**Example**:
```lisp
(return-to-distributed-pool shard-pool "user:123" item)
```

---

### `get-node-for-key`

Get the node that would handle a particular key.

```lisp
(get-node-for-key dist-pool key)
```

**Parameters**:
- `dist-pool` - The distributed pool
- `key` (string) - Key to look up

**Returns**: The pool node that handles this key

**Use cases**:
- Verify key routing for testing
- Get node for manual operations
- Implement custom logic

**Example**:
```lisp
(let ((node (get-node-for-key shard-pool "user:123")))
  (format t "User 123 is on node: ~A" (pool-name node)))
```

---

### `add-node-to-distributed-pool`

Add a new node to a running distributed pool.

```lisp
(add-node-to-distributed-pool dist-pool node-config)
```

**Parameters**:
- `dist-pool` - The distributed pool
- `node-config` (plist) - Configuration for new node (same format as make-distributed-pool)

**Returns**: Modified distributed pool

**Behavior**:
- Creates new node with specified configuration
- Adds node to hash ring
- Some existing keys will redistribute to new node
- No downtime required

**Example**:
```lisp
(add-node-to-distributed-pool
  shard-pool
  (list :name "shard-4" :capacity 15))
```

---

### `remove-node-from-distributed-pool`

Remove a node from a distributed pool.

```lisp
(remove-node-from-distributed-pool dist-pool node-name)
```

**Parameters**:
- `dist-pool` - The distributed pool
- `node-name` (string) - Name of node to remove

**Returns**: Modified distributed pool

**Behavior**:
- Removes node from hash ring
- Remaining keys redistribute to other nodes
- Items in removed node are orphaned (not migrated)

**Important**: For production, migrate data before removing a node.

**Example**:
```lisp
(remove-node-from-distributed-pool shard-pool "shard-2")
```

---

### `with-pool` (with distributed pool)

Macro for distributed pools with automatic cleanup.

```lisp
(with-pool (variable dist-pool key) &body body)
```

**Parameters**:
- `variable` - Symbol to bind fetched item
- `dist-pool` - The distributed pool
- `key` - Key for routing
- `body` - Code to execute

**Behavior**:
- Fetches from appropriate node based on key
- Automatically returns to same node

**Example**:
```lisp
(with-pool (connection shard-pool "user:123")
  (query connection "SELECT * FROM user_data"))
```

---

## Inspecting Pools

These are internal accessors (use `::` syntax) but useful for monitoring:

### Pool Metrics

```lisp
(pooler::pool-current-size pool)        ; Items available now
(pooler::pool-total-created pool)       ; Items ever created
(pooler::pool-total-uses pool)          ; Times items fetched
(pooler::pool-total-pool-inits pool)    ; Times pool refreshed
(pooler::pool-name pool)                ; Pool's name
(pooler::pool-capacity pool)            ; Maximum items
(pooler::pool-threshold pool)           ; Minimum items target
(pooler::pool-timeout pool)             ; Idle timeout (seconds)
```

### Distributed Pool Metrics

```lisp
(pooler::distributed-pool-nodes dist-pool)      ; List of nodes (pools)
(pooler::distributed-pool-hash-ring dist-pool)  ; Hash ring structure
(pooler::distributed-pool-replicas dist-pool)   ; Replicas per node
(pooler::distributed-pool-name dist-pool)       ; Pool name
```

---

## Error Conditions

### `pool-error`

Base condition for all pool errors.

```lisp
(handler-case
  (fetch-from pool)
  (pooler::pool-error (e)
    (format t "Pool error: ~A" e)))
```

### `pool-item-creation-error`

Signaled when the item-maker function fails.

```lisp
(handler-case
  (fetch-from pool)
  (pooler::pool-item-creation-error (e)
    (format t "Could not create item: ~A" e)))
```

### `pool-warning`

Condition signaled when pool-init occurs (usually harmless).

```lisp
(handler-bind
  ((pooler::pool-warning #'(lambda (e) (declare (ignore e)) (invoke-restart 'muffle-warning))))
  (fetch-from pool))  ; Suppress warnings
```

---

## Platform Differences

Pooler abstracts platform differences, but be aware:

- **SBCL**: Uses native `sb-concurrency:queue` (fastest)
- **Allegro CL**: Uses native `mp:queue`
- **Other**: Uses pure Lisp fallback implementation (slightly slower)

All provide the same API and thread-safety guarantees.
