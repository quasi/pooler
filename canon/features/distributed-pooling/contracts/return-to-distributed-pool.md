---
type: contract
name: return-to-distributed-pool
kind: function
status: draft
confidence: 0.8
location: pooler.lisp:169-179
---

# return-to-distributed-pool

Return an item to the distributed pool based on a key.

## Signature

```lisp
(return-to-distributed-pool dist-pool key item) → new-size
```

## Parameters

### dist-pool (required)
- **Type**: `distributed-pool` struct
- **Description**: The distributed pool to return the item to

### key (required)
- **Type**: `string`
- **Description**: Key to determine which node to return to
- **Must Match**: The same key used to fetch the item

### item (required)
- **Type**: `any`
- **Description**: The item to return

## Returns

- **Type**: `fixnum`
- **Description**: The new size of the node's pool (if item was enqueued)

## Behavior

1. Hash the key using `internal-string-hash`
2. Lookup node in hash ring using consistent hashing
3. Delegate to `return-to` on the selected node
4. Return the result from `return-to`

**Critical**: Must use the same key that was used to fetch the item.

## Properties

### Routing Consistency

**Property**: Same key always maps to same node

```
∀ dist-pool, key, item:
  node1 = get-node-for(fetch-from-distributed-pool, key)
  node2 = get-node-for(return-to-distributed-pool, key)
  ⇒ node1 = node2
```

## Examples

### Basic Return

```lisp
(let ((key "user:alice")
      (item (fetch-from-distributed-pool *dist-pool* key)))
  ;; Use item
  (return-to-distributed-pool *dist-pool* key item))
```

### Correct Key Usage

```lisp
;; CORRECT: Same key for fetch and return
(defun process-user-request (user-id)
  (let ((conn (fetch-from-distributed-pool *pool* user-id)))
    (unwind-protect
      (query-data conn user-id)
      (return-to-distributed-pool *pool* user-id conn))))
```

### Incorrect Key Usage

```lisp
;; WRONG: Different keys for fetch and return
(let ((conn (fetch-from-distributed-pool *pool* "user:alice")))
  ;; ...
  (return-to-distributed-pool *pool* "user:bob" conn))  ;; ERROR!
;; Item returned to wrong node!
```

## Error Conditions

### pool-error

Signaled if hash ring is empty (no nodes) or node not found.

## Thread Safety

**Safe**: Uses locks to protect hash ring access and node selection.

## Performance

- **Hashing**: O(1) to hash key
- **Ring Lookup**: O(log N) where N = nodes × replicas
- **Return**: O(1) for underlying pool return

## Common Patterns

### With Unwind-Protect

```lisp
(let ((key "resource:foo")
      (item (fetch-from-distributed-pool *pool* key)))
  (unwind-protect
    (use item)
    (return-to-distributed-pool *pool* key item)))
```

### Helper Function

```lisp
(defun with-distributed-pool-item (key pool fn)
  (let ((item (fetch-from-distributed-pool pool key)))
    (unwind-protect
      (funcall fn item)
      (return-to-distributed-pool pool key item))))

;; Usage
(with-distributed-pool-item "user:alice" *pool*
  (lambda (conn) (query-data conn)))
```

## Key Requirements

**Must be consistent**:
- Use the same key for fetch and return
- Don't transform or hash the key between fetch and return
- Store the key if needed for later return

## See Also

- `fetch-from-distributed-pool` - Fetch item using key
- `get-node-for-key` - Get node for a key
- `return-to` - Underlying pool return (for individual nodes)

## Notes

- **Undocumented**: This function is not in README.md
- **Critical**: Always use the same key for fetch and return
- Incorrect key usage will corrupt pool state (item in wrong node)

## Status

⚠️ **Draft** - Needs user-facing documentation

## Code Location

pooler.lisp:169-179
