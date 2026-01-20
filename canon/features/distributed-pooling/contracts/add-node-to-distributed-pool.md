---
type: contract
name: add-node-to-distributed-pool
kind: function
status: draft
confidence: 0.75
location: pooler.lisp:181-213
---

# add-node-to-distributed-pool

Dynamically add a new node to an existing distributed pool.

## Signature

```lisp
(add-node-to-distributed-pool dist-pool node-config) → dist-pool
```

## Parameters

### dist-pool (required)
- **Type**: `distributed-pool` struct
- **Description**: The distributed pool to add a node to

### node-config (required)
- **Type**: `plist`
- **Description**: Configuration for the new node
- **Required keys**: `:name`, `:item-maker`
- **Optional keys**: `:capacity`, `:threshold`, `:timeout`, `:item-destroyer`

## Returns

- **Type**: `distributed-pool`
- **Description**: The modified distributed pool (same object, mutated)

## Behavior

1. Validate node-config (must have `:name` and `:item-maker`)
2. Check node name doesn't already exist
3. Create new `pool` with specified configuration
4. Add pool to distributed pool's nodes list
5. Add virtual nodes (replicas) to hash ring
6. Sort hash ring by hash value
7. Return modified distributed pool

**Side Effect**: Hash ring is rebuilt, causing key redistribution for some keys.

## Properties

### No Duplicate Names

**Property**: Node names must be unique

```
∀ dist-pool, node-config:
  add-node(dist-pool, node-config)
  ⇒ errors if node-config.name already exists
```

### Hash Ring Update

**Property**: Hash ring size increases

```
∀ dist-pool: old_size = ring.length
  add-node(dist-pool, config)
  ⇒ ring.length = old_size + replicas
```

### Determinism

After adding a node, the hash ring is deterministically sorted, so key routing is consistent.

## Examples

### Add Node to Running Pool

```lisp
(defvar *dist-pool*
  (make-distributed-pool
    :node-configs '((:name "node1") (:name "node2"))
    :item-maker #'make-connection))

;; Later, add capacity
(add-node-to-distributed-pool
  *dist-pool*
  '(:name "node3"
    :item-maker #'make-connection
    :capacity 20))
```

### Add Node with Custom Config

```lisp
(add-node-to-distributed-pool
  *dist-pool*
  '(:name "fast-node"
    :capacity 50
    :threshold 10
    :item-maker #'make-fast-connection
    :item-destroyer #'close-connection))
```

## Error Conditions

### pool-error

**Condition 1**: Missing required keys

```lisp
(handler-case
  (add-node-to-distributed-pool *pool* '(:name "node"))
  (pooler::pool-error (e)
    (format t "Missing item-maker: ~A~%" e)))
```

**Condition 2**: Duplicate node name

```lisp
(handler-case
  (add-node-to-distributed-pool *pool* '(:name "existing-node" ...))
  (pooler::pool-error (e)
    (format t "Node already exists: ~A~%" e)))
```

## Thread Safety

**Safe**: Uses `with-pool-lock` on distributed-pool lock.

Safe to call concurrently, but operations are serialized (one add at a time).

## Performance

- **Time Complexity**: O(N log N) where N = current hash ring size (due to sort)
- **Blocking**: Holds distributed pool lock during entire operation
- **Impact**: Brief disruption to concurrent operations

## Key Redistribution

**Important**: Adding a node causes some keys to reroute.

**Example**:
```
Before: 2 nodes, key "foo" → node1
After add node3: key "foo" might → node3
```

**Consequence**: In-flight operations on rerouted keys may fail or behave unexpectedly.

**Mitigation**: Add nodes during low-traffic periods if possible.

## Use Cases

### Scale Out

```lisp
;; Add capacity during high load
(when (> load threshold)
  (add-node-to-distributed-pool
    *pool*
    '(:name (format nil "node-~A" (get-universal-time))
      :item-maker #'make-worker)))
```

### Heterogeneous Scaling

```lisp
;; Add high-capacity node for specific workloads
(add-node-to-distributed-pool
  *pool*
  '(:name "gpu-node"
    :capacity 100
    :item-maker #'make-gpu-worker))
```

## See Also

- `remove-node-from-distributed-pool` - Remove a node
- `make-distributed-pool` - Create distributed pool with initial nodes
- ADR-0002: Consistent Hashing (why minimal redistribution)

## Notes

- **Undocumented**: Not in README.md
- Key redistribution is partial (not all keys reroute)
- Consistent hashing minimizes redistribution (see ADR-0002)
- No automatic item migration (keys just reroute on next fetch)

## Status

⚠️ **Draft** - Needs user-facing documentation

## Code Location

pooler.lisp:181-213
