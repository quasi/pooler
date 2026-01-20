---
type: contract
name: fetch-from
kind: function
status: stable
confidence: 1.0
location: pooler.lisp:282-291
---

# fetch-from

Retrieve an item from the pool, growing the pool automatically if needed.

## Signature

```lisp
(fetch-from pool &key (tries 2)) → pool-item
```

## Parameters

### pool (required)
- **Type**: `pool` struct
- **Description**: The pool to fetch from

### :tries (optional keyword)
- **Type**: `fixnum` (positive integer)
- **Default**: `2`
- **Description**: Number of retry attempts if pool is empty
- **Purpose**: Handle transient empty conditions during concurrent access

## Returns

- **Type**: `any` (type determined by pool's `item-maker`)
- **Description**: A pool item ready for use

## Behavior

The function implements a retry loop with automatic growth:

```
for attempt in 1..tries:
  item ← fetch-from-aux(pool)

  if item is nil:           # Pool is empty
    grow-pool(pool)         # Auto-grow by threshold
    continue

  if item is :old:          # Pool timed out
    pool-init(pool)         # Re-initialize pool
    continue

  return item               # Success
```

### Side Effects (on success)

- Decrements `pool-current-size`
- Increments `pool-total-uses`
- Updates `pool-last-access` to current time
- Removes item from pool's queue

### Automatic Behaviors

1. **Auto-Growth**: If pool is empty, grows by threshold amount
2. **Auto-Reinit**: If timeout exceeded, calls `pool-init`

## Pre-Conditions

- Pool must be a valid pool object

## Post-Conditions

- An item has been removed from the pool (if successful)
- `pool-current-size` decreased by 1
- `pool-total-uses` increased by 1
- `pool-last-access` updated to current time

## Properties

### Auto-Growth Property

**Property**: Fetching from empty pool triggers growth

```
∀ pool: (pool-current-size = 0) ∧ fetch-from(pool)
  ⇒ grow-pool called
  ⇒ pool-current-size = threshold (after growth)
```

**Verification**: Test fetch-from-empty-pool (pooler-test.lisp:98-105)

### Timeout Property

**Property**: Fetching from timed-out pool triggers re-init

```
∀ pool: (current-time - last-access > timeout) ∧ fetch-from(pool)
  ⇒ pool-init called
  ⇒ all old items destroyed, new items created
```

**Verification**: Test pool-timeout-triggers-reinit (pooler-test.lisp:205-218)

**Design Decision**: See `canon/core/decisions/0001-pool-timeout-lazy-checking.md`

### Usage Tracking

**Property**: Every successful fetch increments total-uses

```
∀ pool:
  old_uses = pool-total-uses
  fetch-from(pool)
  ⇒ pool-total-uses = old_uses + 1
```

**Verification**: Test fetch-from-updates-stats (pooler-test.lisp:116-124)

## Examples

### Basic Usage

```lisp
(defvar *pool* (make-pool :name "Test"))
(let ((item (fetch-from *pool*)))
  ;; Use item
  (return-to *pool* item))
```

### With Custom Retries

```lisp
;; More aggressive retry for high-concurrency scenarios
(let ((item (fetch-from *pool* :tries 5)))
  (when item
    ;; Use item
    (return-to *pool* item)))
```

### Database Connection Example

```lisp
(defvar *db-pool*
  (make-pool
    :item-maker (lambda () (clsql:connect ...))
    :item-destroyer (lambda (conn) (clsql:disconnect :database conn))))

(let ((conn (fetch-from *db-pool*)))
  (clsql:query "SELECT * FROM users" :database conn)
  (return-to *db-pool* conn))
```

## Error Conditions

### pool-item-creation-error

Signaled when `item-maker` fails during auto-growth.

```lisp
(handler-case (fetch-from *pool*)
  (pooler::pool-item-creation-error (e)
    (format t "Failed to create pool item: ~A~%" e)))
```

## Thread Safety

**Safe**: Uses `with-pool-lock` internally for all state modifications.

Multiple threads can call `fetch-from` concurrently without corruption.

## Performance

- **Time Complexity**: O(1) if pool has items, O(threshold) if empty (auto-growth)
- **Blocking**: Holds pool lock briefly during fetch
- **Auto-growth blocking**: If pool is empty, blocks while creating threshold items

## Retry Behavior

The `tries` parameter handles race conditions:

**Scenario**: High concurrency
1. Thread A checks pool (not empty)
2. Thread B fetches last item
3. Thread A tries to fetch (now empty)
4. Thread A retries → auto-grows → succeeds

**Default**: `tries=2` handles most scenarios. Increase for very high concurrency.

## Comparison to Original API

**Historical Note**: In original design (pre-2013), there were two functions:
- `fetch-from` - single attempt
- `fetch-from+` - retry wrapper

These were consolidated into a single `fetch-from` with `:tries` parameter.

**Design Decision**: See `canon/core/decisions/0003-api-consolidation.md`

## Use Cases

### Standard Pool Access

```lisp
(with-pool (conn *db-pool*)
  (clsql:query "..." :database conn))
```

### High-Concurrency Access

```lisp
;; Increase retries for heavy concurrent load
(let ((worker (fetch-from *worker-pool* :tries 5)))
  (funcall worker task)
  (return-to *worker-pool* worker))
```

### Pre-check Pattern

```lisp
;; Check if pool will grow (optional, for monitoring)
(when (zerop (pooler::pool-current-size *pool*))
  (warn "Pool is empty, will auto-grow on fetch"))

(let ((item (fetch-from *pool*)))
  ;; Use item
  )
```

## Common Patterns

### Always Use with-pool

**Recommended**:
```lisp
(with-pool (item *pool*)
  (use item))
```

**Not Recommended** (manual fetch/return):
```lisp
(let ((item (fetch-from *pool*)))
  (unwind-protect
    (use item)
    (return-to *pool* item)))
```

The `with-pool` macro is safer and more idiomatic.

## See Also

- `return-to` - Return item to pool after use
- `with-pool` - Recommended macro for safe fetch/return
- `grow-pool` - Manual pool growth
- `pool-init` - Force pool re-initialization

## Notes

- Timeout checking is lazy (happens during fetch, not continuously)
- Auto-growth creates `threshold` items (not just 1)
- Items are created synchronously (blocks if `item-maker` is slow)
- Pool must not be empty after auto-growth (unless capacity=0)

## Documentation

- README.md:54-56
- CLAUDE.md:102
- Code: pooler.lisp:282-291
- Internal helper: fetch-from-aux (pooler.lisp:267-279, not exported)
