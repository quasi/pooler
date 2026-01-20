---
type: contract
name: grow-pool
kind: function
status: stable
confidence: 1.0
location: pooler.lisp:253-263
---

# grow-pool

Manually grow the pool by creating and adding new items.

## Signature

```lisp
(grow-pool pool &optional grow-by) → nil
```

## Parameters

### pool (required)
- **Type**: `pool` struct
- **Description**: The pool to grow

### grow-by (optional)
- **Type**: `fixnum` (positive integer)
- **Default**: `(pool-threshold pool)` - pool's threshold value
- **Description**: Number of items to add to the pool
- **Constraints**: Must be > 0

## Returns

- **Type**: `nil`
- **Description**: Side-effect only function (modifies pool in place)

## Behavior

For each item to create (up to `grow-by`):
1. Check if pool is at capacity
   - If yes: stop growing (respect capacity limit)
   - If no: continue
2. Call `item-maker` to create new item
3. Enqueue item to pool's queue
4. Increment `pool-total-created`
5. Increment `pool-current-size`
6. Update `pool-last-access` to current time

## Side Effects

- **Modifies**: `pool-current-size`, `pool-total-created`, `pool-last-access`
- **Modifies**: Pool's internal queue (adds items)
- **Calls**: User-supplied `item-maker` function

## Pre-Conditions

- Pool must be a valid pool object
- `grow-by` (if provided) must be a positive integer

## Post-Conditions

- Pool has grown by min(grow-by, capacity - current-size) items
- `pool-current-size` has increased accordingly
- `pool-total-created` has increased by the number of items actually created
- `pool-last-access` is updated to current time

## Properties

### Capacity Enforcement

**Property**: Pool never exceeds capacity

```
∀ pool: grow-pool respects capacity limit
  pool-current-size ≤ pool-capacity (always)
```

**Verification**: Test grow-pool-respects-capacity (pooler-test.lisp:87-94)

### Idempotency

Not idempotent - each call creates new items (unless at capacity).

## Examples

### Grow by Default (Threshold)

```lisp
(defvar *pool* (make-pool :name "Test" :threshold 3))
(grow-pool *pool*)
;; Pool now has 3 items (grew by threshold)
```

### Grow by Specific Amount

```lisp
(defvar *pool* (make-pool :name "Test"))
(grow-pool *pool* 10)
;; Pool now has 10 items
```

### Growth Respects Capacity

```lisp
(defvar *pool* (make-pool :name "Test" :capacity 5))
(grow-pool *pool* 100)
;; Pool has only 5 items (capacity limit enforced)
(assert (= 5 (pooler::pool-current-size *pool*)))
```

## Error Conditions

### pool-item-creation-error

Signaled when `item-maker` function errors during item creation.

**Example**:
```lisp
(defvar *bad-pool*
  (make-pool :item-maker (lambda () (error "Creation failed"))))

(handler-case (grow-pool *bad-pool*)
  (pooler::pool-item-creation-error (e)
    (format t "Failed to create item: ~A~%" e)))
```

## Thread Safety

**Safe**: Uses `with-pool-lock` to protect pool state mutations.

Multiple threads can call `grow-pool` concurrently on the same pool without corruption.

## Performance

- **Time Complexity**: O(n) where n = number of items to create
- **Space Complexity**: O(n) - creates n new items
- **Blocking**: Holds pool lock for entire growth operation

## Use Cases

### Pre-warm Pool

```lisp
;; Create pool with threshold 2 but pre-warm to 10
(defvar *pool* (make-pool :threshold 2 :capacity 20))
(grow-pool *pool* 10)
;; Now have 10 items ready immediately
```

### Recovery After Heavy Use

```lisp
;; After many items destroyed (returned to full pool)
;; Manually grow to restore buffer
(when (< (pooler::pool-current-size *pool*)
         (pooler::pool-threshold *pool*))
  (grow-pool *pool*))
```

## Typical Usage

Generally not called manually - pool auto-grows on `fetch-from` when empty. However, useful for:
- Pre-warming pools before heavy load
- Testing and debugging
- Explicit capacity management

## See Also

- `make-pool` - Create pool with initial configuration
- `fetch-from` - Automatically grows pool when empty
- `pool-init` - Regrows pool to threshold after cleanup

## Notes

- Growth is synchronous and blocks until all items are created
- If `item-maker` is slow, this can block for a long time
- Pool grows by threshold amount automatically on `fetch-from` if empty
- Manual growth is typically only needed for pre-warming

## Documentation

- README.md:42-44
- CLAUDE.md:101
- Code: pooler.lisp:253-263
