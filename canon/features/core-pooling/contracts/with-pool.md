---
type: contract
name: with-pool
kind: macro
status: stable
confidence: 1.0
location: pooler.lisp:323-330
---

# with-pool

Macro for safe pool item borrowing with automatic fetch and return.

## Signature

```lisp
(with-pool (pool-item pool) &body body)
  → result of last form in body
```

## Parameters

### pool-item (symbol)
- **Type**: Symbol (variable name)
- **Description**: Variable to bind the fetched pool item to
- **Scope**: Lexically scoped to the body

### pool (form)
- **Type**: Expression evaluating to a `pool` struct
- **Description**: The pool to fetch from and return to
- **Evaluation**: Evaluated once before fetch

### body (forms)
- **Type**: Lisp forms
- **Description**: Code to execute with the pool item
- **Scope**: Has access to `pool-item` variable

## Returns

- **Type**: `any`
- **Description**: The value of the last form in `body`
- **Note**: Return value is not affected by the automatic return-to

## Expansion

The macro expands to:

```lisp
(let ((pool-item nil))
  (unwind-protect
    (progn
      (setf pool-item (fetch-from pool))
      ,@body)
    (when pool-item
      (return-to pool pool-item))))
```

## Behavior

1. **Initialize**: Bind `pool-item` to `nil`
2. **Fetch**: Call `fetch-from` on the pool
3. **Bind**: Assign fetched item to `pool-item` variable
4. **Execute**: Evaluate body forms with `pool-item` available
5. **Cleanup**: Always call `return-to` (via `unwind-protect`)

### Cleanup Guarantee

The macro uses `unwind-protect` to ensure `return-to` is called even if:
- Body completes normally
- Body signals an error
- Body performs a non-local exit (throw, return-from, etc.)

## Properties

### Cleanup Guarantee

**Property**: Item is always returned to pool

```
∀ pool: with-pool (item pool) body
  ⇒ item is returned to pool
  ⇒ true for normal exit, error, or non-local exit
```

**Verification**:
- Test with-pool-returns-on-error (pooler-test.lisp:235-245)
- Test with-pool-returns-on-normal-exit (pooler-test.lisp:247-255)

### Single Fetch

**Property**: Exactly one fetch-from call per invocation

```
∀ pool: with-pool (item pool) body
  ⇒ fetch-from called exactly once
  ⇒ pool-total-uses increments by 1
```

### Availability

**Property**: Item is available throughout body

```
∀ pool: with-pool (item pool) (use item)
  ⇒ item is non-nil and valid for use
```

## Examples

### Basic Usage

```lisp
(defvar *pool* (make-pool))

(with-pool (item *pool*)
  (print item)
  (do-something-with item))
;; item automatically returned to pool
```

### Database Query

```lisp
(defvar *db-pool*
  (make-pool
    :item-maker (lambda () (clsql:connect ...))
    :item-destroyer (lambda (conn) (clsql:disconnect :database conn))))

(with-pool (conn *db-pool*)
  (clsql:query "SELECT * FROM users" :database conn))
;; Connection automatically returned
```

### Return Value

```lisp
(defvar *pool* (make-pool))

(let ((result (with-pool (item *pool*)
                (compute-something-with item)
                42)))
  (format t "Result: ~A~%" result))
;; Prints "Result: 42"
;; item was returned to pool
```

### Error Handling

```lisp
(handler-case
  (with-pool (conn *db-pool*)
    (clsql:query "INVALID SQL" :database conn))
  (error (e)
    (format t "Query failed: ~A~%" e)))
;; Even though query failed, connection is returned to pool
```

### Nested with-pool

```lisp
(with-pool (worker1 *worker-pool*)
  (with-pool (worker2 *worker-pool*)
    ;; Both workers active
    (parallel-work worker1 worker2)))
;; Both workers returned (inner first, outer second)
```

### Conditional Use

```lisp
(with-pool (item *pool*)
  (when (valid-p item)
    (process item)
    (when (special-case-p item)
      (special-processing item))))
;; Item returned regardless of which branches executed
```

## Thread Safety

**Safe**: Each `with-pool` invocation is independent.

Multiple threads can use `with-pool` concurrently on the same pool:

```lisp
(dotimes (i 10)
  (bt:make-thread
    (lambda ()
      (with-pool (worker *worker-pool*)
        (process-task worker)))))
```

## Performance

- **Time Complexity**: O(1) for fetch and return
- **Overhead**: Minimal (just function calls + unwind-protect)
- **Blocking**: Brief lock acquisition during fetch and return

## Comparison to Manual Management

### with-pool (Recommended)

```lisp
(with-pool (item *pool*)
  (use item))
```

**Advantages**:
- Automatic cleanup
- Exception-safe
- Concise
- Idiomatic

### Manual (Not Recommended)

```lisp
(let ((item (fetch-from *pool*)))
  (unwind-protect
    (use item)
    (return-to *pool* item)))
```

**Disadvantages**:
- More verbose
- Easy to forget cleanup
- Manual unwind-protect needed

## Common Patterns

### Result Transformation

```lisp
(defun get-user-count ()
  (with-pool (conn *db-pool*)
    (caar (clsql:query "SELECT COUNT(*) FROM users"
                       :database conn))))
```

### Conditional Processing

```lisp
(with-pool (item *pool*)
  (cond
    ((needs-processing-p item)
     (process item))
    ((needs-refresh-p item)
     (refresh item))
    (t
     (log:debug "No action needed"))))
```

### Multiple Items from Same Pool

```lisp
(with-pool (item1 *pool*)
  (with-pool (item2 *pool*)
    ;; Now have 2 items from the pool
    (combine-results item1 item2)))
```

## Error Conditions

### pool-item-creation-error

May be signaled during fetch if `item-maker` fails:

```lisp
(handler-case
  (with-pool (item *bad-pool*)
    (use item))
  (pooler::pool-item-creation-error (e)
    (format t "Failed to get item: ~A~%" e)))
```

## Best Practices

### ✓ Do

- Use `with-pool` for all pool item access
- Keep body concise and focused
- Return meaningful values from body
- Use nested `with-pool` for multiple items

### ✗ Don't

- Don't capture `pool-item` and use it after `with-pool` exits
- Don't return `pool-item` itself from body (it will be returned to pool)
- Don't manually call `return-to` inside `with-pool` (it will be returned twice)

### Anti-Pattern: Returning Pool Item

```lisp
;; WRONG - item is returned to pool, then used
(let ((item (with-pool (x *pool*) x)))
  (use item))  ;; ERROR: item already returned

;; CORRECT - use item inside with-pool
(with-pool (item *pool*)
  (use item))
```

## Use Cases

### Resource Management

The primary use case - ensures resources are always released:

```lisp
(with-pool (socket *socket-pool*)
  (write-data socket data)
  (read-response socket))
```

### Transactional Operations

```lisp
(with-pool (conn *db-pool*)
  (clsql:with-transaction (:database conn)
    (clsql:execute-command "INSERT ..." :database conn)
    (clsql:execute-command "UPDATE ..." :database conn)))
```

### Worker Pool Pattern

```lisp
(defun process-batch (items)
  (dolist (item items)
    (with-pool (worker *worker-pool*)
      (funcall worker item))))
```

## See Also

- `fetch-from` - Underlying fetch operation
- `return-to` - Underlying return operation
- `make-pool` - Create pool for use with this macro

## Notes

- This is the recommended way to use pooler in production code
- Always prefer this over manual `fetch-from`/`return-to`
- The macro is exception-safe and handles all edge cases
- `unwind-protect` ensures cleanup even during stack unwinding

## Documentation

- README.md:72-74
- CLAUDE.md:105
- Code: pooler.lisp:323-330
