---
type: contract
name: get-node-for-key
kind: function
status: draft
confidence: 0.8
location: pooler.lisp:141-154
---

# get-node-for-key

Get the pool node (regular pool) responsible for a given key.

## Signature

```lisp
(get-node-for-key dist-pool key) → pool
```

## Parameters

### dist-pool (required)
- **Type**: `distributed-pool` struct
- **Description**: The distributed pool to query

### key (required)
- **Type**: `string`
- **Description**: Key to lookup

## Returns

- **Type**: `pool` struct
- **Description**: The pool object (node) responsible for the key

## Behavior

1. Acquire distributed pool lock (thread-safe access)
2. Hash the key using `internal-string-hash`
3. Find node in hash ring using consistent hashing algorithm
4. Lookup actual pool object by node ID
5. Return the pool

## Properties

### Deterministic Mapping

**Property**: Same key always maps to same node (if topology unchanged)

```
∀ dist-pool, key (no topology changes):
  get-node-for-key(dist-pool, key) = same node (always)
```

### Non-Null Result

**Property**: Always returns a node (or errors)

```
∀ dist-pool, key:
  get-node-for-key(dist-pool, key) ⇒ pool object
  OR signals pool-error
```

## Examples

### Inspect Key Routing

```lisp
(defvar *dist-pool*
  (make-distributed-pool
    :node-configs '((:name "node1") (:name "node2"))
    :item-maker #'make-item))

;; See which node handles a key
(let ((node (get-node-for-key *dist-pool* "user:alice")))
  (format t "Key routes to: ~A~%" (pooler::pool-name node)))
;; Prints: "Key routes to: node1" (or node2, deterministic)
```

### Advanced: Direct Node Access

```lisp
;; Advanced usage: Bypass distributed pool API
(let ((node (get-node-for-key *dist-pool* "user:alice")))
  ;; Use regular pool API directly on node
  (let ((item (pooler:fetch-from node)))
    (use item)
    (pooler:return-to node item)))
;; Not recommended - bypasses distributed pool abstraction
```

### Debugging Key Distribution

```lisp
;; Check distribution of keys across nodes
(defun analyze-distribution (dist-pool keys)
  (let ((node-counts (make-hash-table :test 'equal)))
    (dolist (key keys)
      (let* ((node (get-node-for-key dist-pool key))
             (name (pooler::pool-name node)))
        (incf (gethash name node-counts 0))))
    node-counts))

(analyze-distribution *dist-pool* '("key1" "key2" "key3" ...))
;; Returns: hash table of node-name → count
```

## Error Conditions

### pool-error

**Condition 1**: Hash ring is empty (no nodes)

```lisp
(handler-case
  (get-node-for-key *empty-dist-pool* "key")
  (pooler::pool-error (e)
    (format t "No nodes: ~A~%" e)))
```

**Condition 2**: Node ID found in ring but not in nodes list

This indicates internal inconsistency (should not happen).

## Thread Safety

**Safe**: Uses `with-pool-lock` to protect hash ring and nodes access.

## Performance

- **Hash**: O(1) to hash key
- **Ring Lookup**: O(log N) where N = nodes × replicas (binary search)
- **Node Lookup**: O(M) where M = number of nodes (linear search)

## Use Cases

### Monitoring and Debugging

```lisp
;; Monitor which node handles a specific user
(defun get-user-node (user-id)
  (let ((node (get-node-for-key *user-pool* user-id)))
    (format t "User ~A on node ~A (size: ~A)~%"
            user-id
            (pooler::pool-name node)
            (pooler::pool-current-size node))))
```

### Pre-warming Specific Nodes

```lisp
;; Pre-warm the node that will handle a key
(defun prewarm-for-key (dist-pool key)
  (let ((node (get-node-for-key dist-pool key)))
    (pooler:grow-pool node 10)))

(prewarm-for-key *dist-pool* "important-user")
```

### Custom Routing Logic

```lisp
;; Build custom logic on top of routing
(defun fetch-with-fallback (dist-pool primary-key fallback-key)
  (let ((primary-node (get-node-for-key dist-pool primary-key)))
    (if (> (pooler::pool-current-size primary-node) 0)
        (pooler:fetch-from primary-node)
        ;; Fall back to different node
        (let ((fallback-node (get-node-for-key dist-pool fallback-key)))
          (pooler:fetch-from fallback-node)))))
```

## Comparison to High-Level API

| High-Level | Low-Level (this function) |
|------------|---------------------------|
| `fetch-from-distributed-pool` | `fetch-from (get-node-for-key ...)` |
| Encapsulated | Exposes internals |
| Recommended | Advanced usage only |

## Advanced Pattern: Batching

```lisp
;; Group operations by node for efficiency
(defun batch-fetch-by-node (dist-pool keys)
  (let ((keys-by-node (make-hash-table :test 'equal)))
    ;; Group keys by node
    (dolist (key keys)
      (let* ((node (get-node-for-key dist-pool key))
             (name (pooler::pool-name node)))
        (push key (gethash name keys-by-node nil))))
    ;; Process each node's keys
    (maphash (lambda (node-name keys)
               (let ((node (find node-name
                                 (pooler::distributed-pool-nodes dist-pool)
                                 :key #'pooler::pool-name
                                 :test #'string=)))
                 (process-batch node keys)))
             keys-by-node)))
```

## See Also

- `fetch-from-distributed-pool` - Higher-level fetch using key
- `return-to-distributed-pool` - Higher-level return using key
- ADR-0002: Consistent Hashing (algorithm details)

## Notes

- **Undocumented**: Not in README.md
- **Advanced API**: Most users should use `fetch-from-distributed-pool` instead
- Useful for debugging, monitoring, and advanced custom logic
- Exposes internal node structure (breaks abstraction)

## Status

⚠️ **Draft** - Needs user-facing documentation

## Code Location

pooler.lisp:141-154
