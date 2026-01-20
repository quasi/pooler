---
type: contract
name: make-pool
kind: constructor
status: stable
confidence: 1.0
location: pooler.lisp:48
---

# make-pool

Create a new pool for managing reusable objects.

## Signature

```lisp
(make-pool &key name capacity threshold item-maker item-destroyer timeout)
  → pool
```

## Parameters

### name (optional)
- **Type**: `string`
- **Default**: `"Default Pool"`
- **Description**: Identifying name for the pool (used in error messages and debugging)

### capacity (optional)
- **Type**: `fixnum` (non-negative integer)
- **Default**: `40`
- **Description**: Maximum number of items the pool can hold concurrently
- **Constraints**: Must be > 0
- **Purpose**: Limit resource consumption and prevent unbounded growth

### threshold (optional)
- **Type**: `fixnum` (non-negative integer)
- **Default**: `2`
- **Description**: Minimum number of idle items to maintain in the pool
- **Constraints**: Should be ≤ capacity
- **Purpose**: Ensure availability of pre-created items for immediate use

### item-maker (optional)
- **Type**: `function` with signature `() → item`
- **Default**: `(lambda () 'SAMPLE-ITEM)`
- **Description**: User-supplied function that creates a new pool item
- **Requirements**:
  - Takes no arguments
  - Returns a new pool item
  - Should not error (or pool signals `pool-item-creation-error`)

### item-destroyer (optional)
- **Type**: `function` with signature `(item) → ignored`
- **Default**: `(lambda (item) (setf item nil))`
- **Description**: User-supplied function that cleanly disposes of a pool item
- **Requirements**:
  - Takes one argument (the item to destroy)
  - Performs cleanup (close connections, free resources, etc.)
  - Return value is ignored
  - Errors are silently caught (defensive programming)

### timeout (optional)
- **Type**: `fixnum` (non-negative integer, seconds)
- **Default**: `300` (5 minutes)
- **Description**: Number of seconds of idleness before pool automatically re-initializes
- **Purpose**: Handle stale or expired resources (e.g., database connections that timeout)

## Returns

- **Type**: `pool` struct
- **Description**: A newly created pool object with the specified configuration

## Initial State

The created pool starts in the following state:
- `current-size`: 0 (empty)
- `total-uses`: 0
- `total-created`: 0
- `total-pool-inits`: 0
- `last-access`: 0
- Queue is empty

**Note**: The pool does NOT automatically grow on creation. Items are created lazily on first `fetch-from`.

## Examples

### Basic Pool with Defaults

```lisp
(defvar *pool* (make-pool :name "My Pool"))
;; Creates pool with capacity=40, threshold=2, timeout=300
```

### Database Connection Pool

```lisp
(defvar *db-pool*
  (make-pool
    :name "PostgreSQL Connection Pool"
    :capacity 20
    :threshold 5
    :timeout 600
    :item-maker (lambda ()
                  (clsql:connect '("localhost" "mydb" "user" "pass")
                                 :database-type :postgresql))
    :item-destroyer (lambda (conn)
                      (clsql:disconnect :database conn))))
```

### Thread Pool

```lisp
(defvar *worker-pool*
  (make-pool
    :name "Worker Threads"
    :capacity 10
    :threshold 2
    :item-maker (lambda ()
                  (bt:make-thread (lambda () (worker-loop))
                                  :name "Worker"))
    :item-destroyer (lambda (thread)
                      (bt:destroy-thread thread))))
```

## Properties

### Invariants

- `0 ≤ pool-current-size ≤ pool-capacity` (always maintained)
- Pool name is read-only after creation
- Item-maker and item-destroyer are read-only after creation

### Post-Conditions

- Pool is created with metrics initialized to 0
- Pool is ready to accept `fetch-from` operations
- No items exist in the pool yet (lazy creation)

## Error Conditions

None. This constructor does not signal errors. If `item-maker` is invalid, errors will occur later during `fetch-from` or `grow-pool`.

## Thread Safety

Safe to call concurrently. Each invocation creates an independent pool with its own lock.

## See Also

- `grow-pool` - Manually add items to the pool
- `fetch-from` - Retrieve an item from the pool
- `pool-init` - Re-initialize the pool
- `with-pool` - Macro for safe item borrowing

## Notes

- The default `item-maker` creates the symbol `'SAMPLE-ITEM`, which is only useful for testing
- The default `item-destroyer` is a no-op that sets the item to `nil`
- For production use, always provide meaningful `item-maker` and `item-destroyer` functions
- Timeout checking is lazy (happens on fetch, not continuously) - see ADR-0001

## Historical Context

- **Initial version** (2013-09-12): Implemented with basic pooling
- **Timeout made configurable** (2015-02-23): Added as parameter (commit da82a5e)
- **Default capacity increased** (2013-09-12): Changed from 4 to 40 (commit e7591ac)

## Documentation

- README.md:36-38
- CLAUDE.md:100
- Code: pooler.lisp:48-61
