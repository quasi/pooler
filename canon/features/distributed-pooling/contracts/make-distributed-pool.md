---
type: contract
name: make-distributed-pool
kind: constructor
status: draft
confidence: 0.8
location: pooler.lisp:111-139
extracted_from: triangulation (code + tests + git)
---

# make-distributed-pool

Create a new distributed pool that routes items across multiple nodes using consistent hashing.

## Signature

```lisp
(make-distributed-pool &key name node-configs replicas
                             item-maker item-destroyer
                             default-capacity default-threshold default-timeout)
  → distributed-pool
```

## Parameters

### name (optional)
- **Type**: `string`
- **Default**: `"Default Distributed Pool"`
- **Description**: Identifying name for the distributed pool

### node-configs (optional)
- **Type**: `list` of `plist`
- **Default**: `nil`
- **Description**: List of node configuration plists
- **Schema**: Each plist can contain:
  - `:name` - node name (string)
  - `:capacity` - max items for this node (fixnum)
  - `:threshold` - min idle items for this node (fixnum)
  - `:timeout` - timeout seconds for this node (fixnum)
  - `:item-maker` - creation function for this node
  - `:item-destroyer` - destruction function for this node

### replicas (optional)
- **Type**: `fixnum` (positive integer)
- **Default**: `10`
- **Description**: Number of virtual nodes per physical node on the hash ring
- **Purpose**: Improve distribution uniformity
- **Trade-off**: More replicas = better distribution but larger hash ring

### item-maker (optional)
- **Type**: `function` with signature `() → item`
- **Default**: `nil`
- **Description**: Default item-maker for nodes that don't specify one
- **Note**: Either global or per-node item-maker is required

### item-destroyer (optional)
- **Type**: `function` with signature `(item) → ignored`
- **Default**: `nil`
- **Description**: Default item-destroyer for nodes that don't specify one

### default-capacity (optional)
- **Type**: `fixnum`
- **Default**: `40`
- **Description**: Default capacity for nodes that don't specify capacity

### default-threshold (optional)
- **Type**: `fixnum`
- **Default**: `2`
- **Description**: Default threshold for nodes that don't specify threshold

### default-timeout (optional)
- **Type**: `fixnum`
- **Default**: `300`
- **Description**: Default timeout for nodes that don't specify timeout

## Returns

- **Type**: `distributed-pool` struct
- **Description**: A newly created distributed pool with configured nodes and hash ring

## Behavior

1. Create distributed pool structure
2. For each node-config:
   - Create a regular `pool` with specified or default parameters
   - Require `:name` and `:item-maker` (global or per-node)
3. Build hash ring:
   - For each node, add `replicas` virtual nodes to hash ring
   - Sort hash ring by hash value
4. Return configured distributed pool

## Error Conditions

### pool-error

Signaled when `item-maker` is not provided globally or for a specific node.

```lisp
(handler-case
  (make-distributed-pool :node-configs '((:name "node1")))
  (pooler::pool-error (e)
    (format t "Missing item-maker: ~A~%" e)))
```

## Examples

### Basic Distributed Pool

```lisp
(defvar *dist-pool*
  (make-distributed-pool
    :name "Distributed Connection Pool"
    :node-configs '((:name "node1")
                    (:name "node2")
                    (:name "node3"))
    :item-maker (lambda () (create-connection))
    :item-destroyer (lambda (conn) (close-connection conn))))
```

### Heterogeneous Nodes

```lisp
(defvar *dist-pool*
  (make-distributed-pool
    :name "Mixed Pool"
    :node-configs
      '((:name "fast-node" :capacity 20 :threshold 5)
        (:name "slow-node" :capacity 10 :threshold 2
         :item-maker #'slow-connection-maker))
    :item-maker #'fast-connection-maker
    :item-destroyer #'close-connection))
```

### High Distribution Quality

```lisp
;; Use more replicas for better distribution
(defvar *dist-pool*
  (make-distributed-pool
    :replicas 20  ; More virtual nodes per physical node
    :node-configs '((:name "n1") (:name "n2"))
    :item-maker #'make-item))
```

## Properties

### Hash Ring Structure

**Property**: Hash ring size equals nodes × replicas

```
∀ dist-pool:
  hash-ring.length = nodes.length × replicas
```

**Property**: Hash ring is sorted by hash value

```
∀ dist-pool:
  hash-ring sorted ascending by hash-value
```

## Thread Safety

Safe to call concurrently. Each invocation creates an independent distributed pool.

## See Also

- `fetch-from-distributed-pool` - Get item using key
- `return-to-distributed-pool` - Return item using key
- `add-node-to-distributed-pool` - Add node dynamically
- `remove-node-from-distributed-pool` - Remove node
- ADR-0002: Consistent Hashing (design rationale)

## Notes

- **Undocumented**: This function is not documented in README.md
- Nodes are regular `pool` structs with full pool capabilities
- Virtual nodes improve distribution uniformity (more replicas = better distribution)
- See ADR-0002 for rationale on consistent hashing choice

## Historical Context

- **Added** (2025-06-09): Implemented by google-labs-jules[bot] (commit cead997)
- AI-generated feature with comprehensive commit message

## Status

⚠️ **Draft** - Needs user-facing documentation in README.md

## Code Location

pooler.lisp:111-139
