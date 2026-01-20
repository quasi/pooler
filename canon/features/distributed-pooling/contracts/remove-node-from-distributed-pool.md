---
type: contract
name: remove-node-from-distributed-pool
kind: function
status: draft
confidence: 0.7
location: pooler.lisp:215-236
---

# remove-node-from-distributed-pool

Dynamically remove a node from an existing distributed pool.

## Signature

```lisp
(remove-node-from-distributed-pool dist-pool node-name-to-remove) → dist-pool
```

## Parameters

### dist-pool (required)
- **Type**: `distributed-pool` struct
- **Description**: The distributed pool to remove a node from

### node-name-to-remove (required)
- **Type**: `string`
- **Description**: Name of the node to remove

## Returns

- **Type**: `distributed-pool`
- **Description**: The modified distributed pool (same object, mutated)

## Behavior

1. Validate node exists (error if not found)
2. Remove node from distributed pool's nodes list
3. Remove all virtual nodes (replicas) for this node from hash ring
4. Return modified distributed pool

⚠️ **Important**: Items in the removed node's pool are **NOT** destroyed or migrated.

## Limitation

**Known Limitation**: This implementation does NOT handle draining items from the removed node.

**Consequence**: Items in the removed pool are orphaned until manual cleanup.

**Code Comment** (pooler.lisp:229-231):
```
;; NOTE: This version does not handle draining items from the pool being removed.
;; Items in the removed pool will be orphaned until its pool-init or manual cleanup.
```

**Design Decision**: See `canon/core/decisions/0004-node-removal-limitation.md`

## Properties

### Node Removal

**Property**: Node no longer in nodes list or hash ring

```
∀ dist-pool, node-name:
  remove-node(dist-pool, node-name)
  ⇒ node-name not in dist-pool.nodes
  ⇒ node-name not in dist-pool.hash-ring
```

### Hash Ring Update

**Property**: Hash ring size decreases

```
∀ dist-pool: old_size = ring.length
  remove-node(dist-pool, node-name)
  ⇒ ring.length = old_size - replicas
```

## Examples

### Remove Node

```lisp
(defvar *dist-pool*
  (make-distributed-pool
    :node-configs '((:name "node1") (:name "node2") (:name "node3"))
    :item-maker #'make-connection))

;; Remove node2
(remove-node-from-distributed-pool *dist-pool* "node2")
;; node2's pool still exists but is orphaned
;; Keys that mapped to node2 now map to other nodes
```

### Manual Cleanup Before Removal

```lisp
;; Recommended: Manually drain node before removal
(let ((node (find "node2" (pooler::distributed-pool-nodes *dist-pool*)
                  :key #'pooler::pool-name :test #'string=)))
  ;; Wait for items to be returned naturally
  (loop while (> (pooler::pool-current-size node) 0)
        do (sleep 1))
  ;; Now remove
  (remove-node-from-distributed-pool *dist-pool* "node2"))
```

## Error Conditions

### pool-error

Signaled if node not found.

```lisp
(handler-case
  (remove-node-from-distributed-pool *pool* "nonexistent-node")
  (pooler::pool-error (e)
    (format t "Node not found: ~A~%" e)))
```

## Thread Safety

**Safe**: Uses `with-pool-lock` on distributed-pool lock.

## Performance

- **Time Complexity**: O(N) where N = hash ring size (to filter replicas)
- **Blocking**: Holds distributed pool lock during entire operation

## Key Redistribution

**Important**: Removing a node causes keys that mapped to it to reroute.

**Example**:
```
Before: 3 nodes, key "foo" → node2
Remove node2
After: 2 nodes, key "foo" → node1 or node3
```

**Consequence**: Next fetch for rerouted keys goes to different node.

## Orphaned Items

**Problem**: Items in removed node's pool are not destroyed or migrated.

**Workarounds**:

### Option 1: Manual Drain Before Removal

```lisp
;; Drain node before removing
(defun drain-node (dist-pool node-name)
  (let ((node (find node-name (pooler::distributed-pool-nodes dist-pool)
                    :key #'pooler::pool-name :test #'string=)))
    ;; Force pool-init to destroy items
    (pooler:pool-init node)
    ;; Now remove
    (remove-node-from-distributed-pool dist-pool node-name)))
```

### Option 2: Accept Orphaning

```lisp
;; Just remove, items will be GC'd eventually
(remove-node-from-distributed-pool *dist-pool* "old-node")
;; Orphaned items will be garbage collected when pool is GC'd
```

## Use Cases

### Scale Down

```lisp
;; Remove excess capacity during low load
(when (< load low-threshold)
  (remove-node-from-distributed-pool *pool* "extra-node"))
```

### Maintenance

```lisp
;; Remove node for maintenance
(remove-node-from-distributed-pool *pool* "node-to-upgrade")
;; Perform maintenance
;; Add back with add-node-to-distributed-pool
```

## Comparison to Add Node

| Add Node | Remove Node |
|----------|-------------|
| Increases capacity | Decreases capacity |
| Minimal key redistribution | Reroutes keys from removed node |
| No item issues | ⚠️ Orphans items |

## Future Work

Potential enhancements (see ADR-0004):
- Automatic item draining
- Item migration to other nodes
- Graceful shutdown with cooldown period

## See Also

- `add-node-to-distributed-pool` - Add a node
- `make-distributed-pool` - Create distributed pool
- ADR-0004: Node Removal Limitation (design rationale)

## Notes

- **Undocumented**: Not in README.md
- **Limitation**: Does not drain or migrate items
- **Recommendation**: Manually drain before removal if items matter
- Consistent hashing minimizes key redistribution (see ADR-0002)

## Status

⚠️ **Draft** - Needs user-facing documentation and limitation documentation

## Code Location

pooler.lisp:215-236
