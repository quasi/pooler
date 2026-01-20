---
type: contract
name: fetch-from-distributed-pool
kind: function
status: draft
confidence: 0.8
location: pooler.lisp:156-167
---

# fetch-from-distributed-pool

Retrieve an item from the distributed pool based on a key.

## Signature

```lisp
(fetch-from-distributed-pool dist-pool key &key (tries 2))
  → pool-item
```

## Parameters

### dist-pool (required)
- **Type**: `distributed-pool` struct
- **Description**: The distributed pool to fetch from

### key (required)
- **Type**: `string`
- **Description**: Key to determine which node to fetch from
- **Purpose**: Consistently routes to same node for same key

### :tries (optional keyword)
- **Type**: `fixnum` (positive integer)
- **Default**: `2`
- **Description**: Number of retry attempts (passed to underlying `fetch-from`)

## Returns

- **Type**: `any` (type determined by node's `item-maker`)
- **Description**: A pool item from the node responsible for the key

## Behavior

1. Hash the key using `internal-string-hash`
2. Lookup node in hash ring using consistent hashing
3. Delegate to `fetch-from` on the selected node
4. Return the fetched item

**Key Insight**: Same key always maps to same node (unless topology changes).

## Properties

### Deterministic Routing

**Property**: Same key always maps to same node

```
∀ dist-pool, key (topology unchanged):
  fetch-from-distributed-pool(dist-pool, key)
  ⇒ always uses same node
```

### Consistent Hashing

Uses consistent hashing algorithm for node selection - see ADR-0002.

## Examples

### Basic Usage

```lisp
(defvar *dist-pool* (make-distributed-pool ...))

;; Fetch item for user "alice"
(let ((item (fetch-from-distributed-pool *dist-pool* "user:alice")))
  ;; Use item
  (return-to-distributed-pool *dist-pool* "user:alice" item))
```

### User-Specific Resources

```lisp
;; Each user always gets routed to same node
(defun get-user-connection (user-id)
  (fetch-from-distributed-pool *db-pool* user-id))

(let ((conn (get-user-connection "alice")))
  (query-user-data conn "alice")
  (return-to-distributed-pool *db-pool* "alice" conn))
```

### Session Affinity

```lisp
;; Session always maps to same backend
(defun get-session-handler (session-id)
  (fetch-from-distributed-pool *handler-pool* session-id))
```

## Error Conditions

### pool-error

Signaled if hash ring is empty (no nodes).

```lisp
(handler-case
  (fetch-from-distributed-pool *empty-dist-pool* "key")
  (pooler::pool-error (e)
    (format t "No nodes available: ~A~%" e)))
```

### pool-item-creation-error

Signaled if the selected node's `item-maker` fails.

## Thread Safety

**Safe**: Uses locks to protect hash ring access and node selection.

Multiple threads can call concurrently with different keys safely.

## Performance

- **Hashing**: O(1) to hash key
- **Ring Lookup**: O(log N) where N = nodes × replicas (binary search on sorted ring)
- **Fetch**: O(1) for underlying pool fetch

## Key Design

**Recommendations for keys**:
- Use meaningful identifiers (user IDs, session IDs, resource names)
- Ensure keys are consistent (same user = same key)
- Avoid sequential keys if you want distribution (use hash if needed)

**Examples**:
```lisp
;; Good keys
"user:12345"
"session:a3f2b1c"
"resource:foo"

;; Poor keys (sequential = poor distribution)
"1", "2", "3", ...
```

## Comparison to Basic Pool

| Basic Pool | Distributed Pool |
|------------|------------------|
| `fetch-from pool` | `fetch-from-distributed-pool dist-pool key` |
| No key needed | Key required for routing |
| Single pool | Multiple pools (nodes) |

## Use Cases

### Sharded Resources

```lisp
;; Database connections sharded by user ID
(let ((conn (fetch-from-distributed-pool *db-pool* user-id)))
  (query-user-data conn user-id))
```

### Session Management

```lisp
;; Session handlers with affinity
(let ((handler (fetch-from-distributed-pool *session-pool* session-id)))
  (process-session handler session-id))
```

### Cache Sharding

```lisp
;; Cache entries distributed across nodes
(let ((cache-shard (fetch-from-distributed-pool *cache-pool* cache-key)))
  (get-from-cache cache-shard cache-key))
```

## See Also

- `return-to-distributed-pool` - Return item to distributed pool
- `get-node-for-key` - Get the node for a key (advanced)
- `make-distributed-pool` - Create distributed pool
- ADR-0002: Consistent Hashing

## Notes

- **Undocumented**: This function is not in README.md
- Key must be used for both fetch and return to ensure correct node
- Node topology changes (add/remove) may reroute some keys

## Status

⚠️ **Draft** - Needs user-facing documentation

## Code Location

pooler.lisp:156-167
