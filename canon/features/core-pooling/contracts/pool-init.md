---
type: contract
name: pool-init
kind: function
status: stable
confidence: 1.0
location: pooler.lisp:306-320
---

# pool-init

Re-initialize the pool by destroying all existing items and creating fresh ones.

## Signature

```lisp
(pool-init pool) → nil
```

## Parameters

### pool (required)
- **Type**: `pool` struct
- **Description**: The pool to re-initialize

## Returns

- **Type**: `nil`
- **Description**: Side-effect only function (modifies pool in place)

## Behavior

Pool re-initialization follows this sequence:

```
1. DESTROY phase:
   while items exist in pool:
     item ← fetch-from-aux(pool, old=true)
     destroy-pool-item(item)
     decrement pool-current-size

2. RESET phase:
   pool-queue ← new empty queue
   pool-current-size ← 0
   pool-total-uses ← 0
   pool-total-created ← 0
   increment pool-total-pool-inits
   signal pool-warning

3. REGROWTH phase:
   grow-pool(pool)  # Grows to threshold
```

## Side Effects

### Modifications
- Destroys all existing pool items (calls `item-destroyer` for each)
- Creates new empty queue
- Resets metrics: `current-size`, `total-uses`, `total-created` to 0
- Increments `total-pool-inits` counter
- Regrows pool to `threshold` size

### Signals
- `pool-warning` condition with message: `"{pool-name} POOL-INIT no.{count}"`

## Pre-Conditions

- Pool must be a valid pool object

## Post-Conditions

- All old pool items have been destroyed
- Pool has `threshold` new items
- `pool-current-size` = `threshold`
- `pool-total-uses` = 0
- `pool-total-created` = `threshold` (from regrowth)
- `pool-total-pool-inits` increased by 1
- `pool-warning` has been signaled

## Properties

### Destruction Guarantee

**Property**: All items are destroyed before regrowth

```
∀ pool:
  old_items = all items in pool
  pool-init(pool)
  ⇒ item-destroyer called for each item in old_items
  ⇒ old_items not in new pool
```

**Verification**: Test pool-init-destroys-all-items (pooler-test.lisp:167-179)

### Counter Reset

**Property**: Metrics reset except total-pool-inits

```
∀ pool: pool-init(pool)
  ⇒ pool-current-size = threshold (after regrowth)
  ⇒ pool-total-uses = 0
  ⇒ pool-total-created = threshold (after regrowth)
  ⇒ pool-total-pool-inits increased by 1
```

**Verification**: Test pool-init-resets-counters (pooler-test.lisp:181-193)

### Regrowth

**Property**: Pool regrows to threshold after cleanup

```
∀ pool: pool-init(pool)
  ⇒ pool-current-size = pool-threshold
  ⇒ pool has threshold fresh items
```

**Verification**: Test pool-init-regrows-to-threshold (pooler-test.lisp:195-201)

### Warning Signal

**Property**: pool-warning always signaled

```
∀ pool: pool-init(pool)
  ⇒ pool-warning signaled with message
```

**Purpose**: Notify monitoring systems of pool churn events

## Examples

### Manual Re-initialization

```lisp
(defvar *pool* (make-pool))
(grow-pool *pool* 10)

;; Force pool refresh
(pool-init *pool*)
;; Pool now has threshold (2) fresh items
;; Old 10 items destroyed
```

### Handle Pool Warning

```lisp
(handler-bind ((pooler::pool-warning
                (lambda (w)
                  (log:warn "Pool re-init: ~A" (pooler::message w))
                  (muffle-warning))))
  (pool-init *pool*))
```

### Database Pool Refresh

```lisp
(defvar *db-pool*
  (make-pool
    :threshold 5
    :item-maker (lambda () (clsql:connect ...))
    :item-destroyer (lambda (conn) (clsql:disconnect :database conn))))

;; Periodically refresh connections (e.g., after schema change)
(pool-init *db-pool*)
;; All old connections closed
;; 5 new connections created
```

### Monitoring Pool Churn

```lisp
(let ((before (pooler::pool-total-pool-inits *pool*)))
  (pool-init *pool*)
  (let ((after (pooler::pool-total-pool-inits *pool*)))
    (format t "Pool has been re-initialized ~A times total~%"
            after)))
```

## Triggers

### Automatic Trigger

Pool-init is automatically called by `fetch-from` when timeout exceeded:

```lisp
(defvar *pool* (make-pool :timeout 5))
(grow-pool *pool* 10)

(sleep 6)  ; Exceed timeout

(fetch-from *pool*)  ; Automatically triggers pool-init
```

**Design Decision**: See `canon/core/decisions/0001-pool-timeout-lazy-checking.md`

### Manual Trigger

Useful for:
- Forcing connection refresh after network issues
- Clearing pool after configuration changes
- Testing and debugging

## Error Conditions

### pool-item-creation-error

May be signaled during regrowth phase if `item-maker` fails.

```lisp
(handler-case (pool-init *pool*)
  (pooler::pool-item-creation-error (e)
    (format t "Failed to create new items during init: ~A~%" e)))
```

## Thread Safety

**Safe**: Uses `with-pool-lock` to protect all operations.

However, pool-init is a heavyweight operation - concurrent `fetch-from` calls will block.

## Performance

- **Time Complexity**: O(n) where n = number of items to destroy + threshold
- **Blocking**: Holds pool lock for entire operation
- **Synchronous**: Blocks until all items destroyed and regrown

**Warning**: For large pools with slow `item-destroyer`, this can block for a long time.

## Use Cases

### Periodic Refresh

```lisp
;; Refresh database connections every hour
(bt:make-thread
  (lambda ()
    (loop
      (sleep 3600)
      (handler-bind ((pooler::pool-warning #'muffle-warning))
        (pool-init *db-pool*)))))
```

### Error Recovery

```lisp
;; After detecting widespread connection errors
(when (> error-rate threshold)
  (log:info "High error rate, refreshing pool")
  (pool-init *connection-pool*))
```

### Configuration Change

```lisp
;; After database failover
(setf *db-config* new-config)
(pool-init *db-pool*)  ; Force reconnection with new config
```

## Warning Handling

The `pool-warning` is informational. Common patterns:

### Log and Continue

```lisp
(handler-bind ((pooler::pool-warning
                (lambda (w)
                  (log:info "Pool init: ~A" (pooler::message w))
                  (muffle-warning))))
  (pool-init *pool*))
```

### Muffle in Tests

```lisp
(handler-bind ((warning #'muffle-warning))
  (pool-init *pool*))
```

## See Also

- `fetch-from` - Triggers pool-init automatically on timeout
- `grow-pool` - Add items without destroying existing ones
- `make-pool` - Set timeout parameter
- `pool-warning` condition

## Notes

- Pool-init is a disruptive operation (destroys all items)
- Use sparingly in production (prefer automatic timeout-based refresh)
- Warning is always signaled (use handlers to suppress if needed)
- Regrowth happens immediately (not lazy)
- Items are destroyed before metrics are reset (proper cleanup order)

## Historical Context

- **Bug fix** (2015-02-23): Ensured all items are destroyed before regrowth (commit 1426134)
  - Previous version had a bug where some items weren't properly destroyed
  - Fixed by passing `old=true` parameter to `fetch-from-aux`

## Documentation

- README.md:66-68
- CLAUDE.md:104
- Code: pooler.lisp:306-320
