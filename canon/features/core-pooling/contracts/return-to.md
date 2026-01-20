---
type: contract
name: return-to
kind: function
status: stable
confidence: 1.0
location: pooler.lisp:295-303
---

# return-to

Return a pool item back to the pool after use.

## Signature

```lisp
(return-to pool pool-item) → new-size
```

## Parameters

### pool (required)
- **Type**: `pool` struct
- **Description**: The pool to return the item to

### pool-item (required)
- **Type**: `any` (matches type created by pool's `item-maker`)
- **Description**: The item to return
- **Constraints**: Should be an item originally fetched from this pool

## Returns

- **Type**: `fixnum`
- **Description**: The new `pool-current-size` (only if item was enqueued)
- **Note**: Return value may be inconsistent if item was destroyed

## Behavior

The function implements capacity-aware return logic:

```
if pool-current-size < pool-capacity:
  enqueue(pool-item)
  increment pool-current-size
  update pool-last-access to current-time
  return pool-current-size
else:
  destroy-pool-item(pool-item)
  # Do NOT update last-access
  # Item is destroyed, not stored
```

### Side Effects

**If item is enqueued** (pool below capacity):
- Adds item to pool's queue
- Increments `pool-current-size`
- Updates `pool-last-access` to current time

**If item is destroyed** (pool at capacity):
- Calls `item-destroyer(pool-item)` (errors ignored)
- Does NOT increment `pool-current-size`
- Does NOT update `pool-last-access` ⚠️

## Pre-Conditions

- Pool must be a valid pool object
- Item should be compatible with pool's `item-destroyer`

## Post-Conditions

**If enqueued**:
- Item is back in pool and available for future `fetch-from`
- `pool-current-size` increased by 1
- `pool-last-access` updated

**If destroyed**:
- Item's resources are cleaned up
- Item is no longer tracked by pool
- Pool state unchanged

## Properties

### Capacity Enforcement

**Property**: Returning to full pool destroys item

```
∀ pool, item:
  (pool-current-size = pool-capacity) ∧ return-to(pool, item)
  ⇒ item is destroyed
  ⇒ pool-current-size unchanged
```

**Verification**: Test return-to-respects-capacity (pooler-test.lisp:137-153)

### Size Invariant

**Property**: Pool size never exceeds capacity

```
∀ pool, item: return-to(pool, item)
  ⇒ pool-current-size ≤ pool-capacity
```

**Verification**: Multiple tests verify this invariant

### Idempotency

**Not idempotent**: Returning the same item multiple times is unsafe and can corrupt the pool (same item enqueued multiple times).

**User Responsibility**: Only return items once.

## Examples

### Basic Return

```lisp
(let ((item (fetch-from *pool*)))
  ;; Use item
  (return-to *pool* item))
```

### Return to Full Pool

```lisp
(defvar *pool* (make-pool :capacity 2))
(grow-pool *pool* 2)  ;; Pool is full

;; Create item outside pool
(let ((extra-item (funcall (pooler::pool-item-maker *pool*))))
  (return-to *pool* extra-item))
  ;; extra-item is destroyed (pool was full)
  ;; pool-current-size still 2
```

### Database Connection Return

```lisp
(defvar *db-pool*
  (make-pool
    :capacity 10
    :item-maker (lambda () (clsql:connect ...))
    :item-destroyer (lambda (conn) (clsql:disconnect :database conn))))

(let ((conn (fetch-from *db-pool*)))
  (clsql:query "SELECT * FROM users" :database conn)
  (return-to *db-pool* conn))
;; If pool was at capacity, connection is closed
;; If pool had space, connection is reused
```

## Error Conditions

None. Errors in `item-destroyer` are silently caught (`ignore-errors`).

**Design Rationale**: Defensive programming to prevent `item-destroyer` bugs from crashing the pool.

**Caveat**: Silent failures may hide resource leaks or bugs in user-provided `item-destroyer`.

## Thread Safety

**Safe**: Uses `with-pool-lock` to protect pool state modifications.

Multiple threads can call `return-to` concurrently without corruption.

## Performance

- **Time Complexity**: O(1)
- **Blocking**: Holds pool lock briefly
- **Item destruction**: May block if `item-destroyer` is slow (but uses `ignore-errors`)

## Behavioral Notes

### Last-Access Asymmetry

**Important**: `return-to` does NOT update `last-access` when the item is destroyed (pool at capacity).

This can cause the pool to timeout sooner than expected if many items are returned to a full pool.

**Observation**: See .canon-initiation observations (obs-prop-002) - potential bug or intentional?

### Item Ownership

After calling `return-to`, the item belongs to the pool again. The caller should not use the item further.

**Bad Example**:
```lisp
(let ((item (fetch-from *pool*)))
  (return-to *pool* item)
  (use-item item))  ;; WRONG! Item may have been destroyed or reused
```

## Common Patterns

### Always Pair with Fetch

```lisp
;; Pattern 1: Manual (not recommended)
(let ((item (fetch-from *pool*)))
  (unwind-protect
    (use item)
    (return-to *pool* item)))

;; Pattern 2: with-pool macro (recommended)
(with-pool (item *pool*)
  (use item))
```

### Conditional Return

```lisp
(let ((item (fetch-from *pool*)))
  (if (valid-item-p item)
      (progn
        (use item)
        (return-to *pool* item))
      (progn
        ;; Item is invalid, destroy it
        (funcall (pooler::pool-item-destroyer *pool*) item))))
```

## Use Cases

### Standard Pool Usage

Used at the end of pool item lifecycle to make item available again.

### Resource Cleanup

If pool is at capacity, `return-to` acts as a cleanup mechanism (destroys excess items).

### Load Shedding

Returning to a full pool effectively discards the item, which can be used for load shedding:
- Create many workers during high load
- Return them when load decreases
- Excess workers are automatically cleaned up

## See Also

- `fetch-from` - Get item from pool
- `with-pool` - Macro that handles fetch and return automatically
- `pool-init` - Re-initialize pool (destroys all items)

## Notes

- Always return items fetched from the pool
- Don't use items after returning them
- Silent `item-destroyer` errors may hide bugs
- Prefer `with-pool` macro over manual fetch/return

## Documentation

- README.md:60-62
- CLAUDE.md:103
- Code: pooler.lisp:295-303
