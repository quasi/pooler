---
type: decision
adr: "0002"
title: Consistent Hashing for Distributed Pool
status: accepted
date: 2025-06-09
---

# ADR-0002: Consistent Hashing for Distributed Pool

## Status

**Accepted** (implemented 2025-06-09)

## Context

The distributed pool feature enables horizontal scaling by distributing pooled items across multiple underlying pool instances (nodes). The core challenge is: **How do we consistently map a key to a node?**

Requirements:
1. **Deterministic**: Same key always maps to same node (for session affinity, sharding)
2. **Balanced**: Keys should distribute roughly evenly across nodes
3. **Minimal disruption**: Adding/removing nodes should remap as few keys as possible
4. **Simple**: Implementation should be straightforward and maintainable

The key-to-node mapping algorithm fundamentally impacts:
- **Key redistribution**: How many keys reroute when topology changes
- **Load balancing**: How evenly keys distribute across nodes
- **Predictability**: How easy it is to reason about which keys map where

## Decision

**Use consistent hashing with virtual nodes (replicas) for key-to-node mapping.**

### Implementation

From commit cead997 (2025-06-09):

1. **Hash Ring**: Sorted array of (hash-value, node-id) pairs representing virtual nodes
2. **Virtual Nodes**: Each physical node gets multiple positions on ring (default: 10 replicas)
3. **Key Lookup**: Hash the key, binary search ring for next highest hash value
4. **Node Addition**: Insert new node's virtual nodes into sorted ring
5. **Node Removal**: Remove node's virtual nodes from ring

```lisp
;; Hash ring node structure
(defstruct hash-ring-node
  hash    ; Hash value for this virtual node
  node-id ; ID of the physical node
)

;; Key lookup
(defun get-node-for-key (dist-pool key)
  (let* ((key-hash (internal-string-hash key))
         (ring (distributed-pool-hash-ring dist-pool))
         (ring-position (find-next-node-position key-hash ring)))
    (lookup-pool-by-id (hash-ring-node-node-id (aref ring ring-position)))))
```

### Configuration

- **Default replicas**: 10 virtual nodes per physical node
- **Hashing function**: `sxhash` (Common Lisp built-in)
  - Code comment notes: "Replace with a more robust one if needed"
  - Simple and portable across CL implementations

## Alternatives Considered

### Alternative 1: Simple Modulo Hashing

**Approach**: `node-index = (hash key) mod (number-of-nodes)`

**Example**:
```
3 nodes: A, B, C
key "user:alice" → hash=42 → 42 mod 3 = 0 → node A
key "user:bob"   → hash=17 → 17 mod 3 = 2 → node C
```

**Pros**:
- Extremely simple implementation (single modulo operation)
- Fast lookup (O(1), no binary search needed)
- Easy to understand and debug

**Cons**:
- **Massive key redistribution on topology changes**:
  - Add/remove node → ALL keys potentially remap
  - Example: Add 4th node → `hash mod 4` instead of `hash mod 3` → ~75% of keys remap
- **Poor load balancing**: Hash distribution may be uneven
- **Unpredictable**: Adding node C affects keys that map to A and B

**Why rejected**: Topology changes are common operations (scaling up/down, maintenance). Remapping 75% of keys is unacceptable disruption.

**Math**: With N nodes, adding one node causes `(N/(N+1))` keys to remap. For N=3, that's 75%. For N=9, that's 90%.

### Alternative 2: Rendezvous Hashing (Highest Random Weight)

**Approach**: For each key, hash `(key, node)` for every node, pick node with highest hash.

**Example**:
```
key "user:alice"
  hash(alice, nodeA) = 42
  hash(alice, nodeB) = 17
  hash(alice, nodeC) = 99  ← highest, pick C
```

**Pros**:
- Minimal redistribution on changes (only affected keys remap)
- No data structures needed (stateless)
- Better load balancing than modulo

**Cons**:
- **O(N) lookup**: Must hash key against every node
- **Slow at scale**: 100 nodes = 100 hash operations per lookup
- More complex to implement than consistent hashing

**Why rejected**: Lookup performance degrades linearly with node count. Consistent hashing is O(log N).

### Alternative 3: Jump Hash

**Approach**: Google's jump consistent hash algorithm (deterministic, O(1) lookup).

**Pros**:
- Very fast (O(1) lookup, no data structures)
- Minimal redistribution
- Simple algorithm

**Cons**:
- **Assumes integer node IDs**: Nodes must be numbered 0, 1, 2, ... N-1
- **Cannot remove arbitrary nodes**: Only supports removing last node (N-1)
- **Poor fit for dynamic topology**: Adding/removing nodes mid-range requires renumbering
- Less known in Common Lisp ecosystem

**Why rejected**: Distributed pool needs arbitrary node add/remove. Jump hash's constraints are too restrictive.

## Consequences

### Positive

1. **Minimal key redistribution**:
   - Adding node N to N-1 nodes: only `1/N` of keys remap (e.g., 3→4 nodes: ~25%)
   - Removing node: only keys on that node remap to other nodes
   - Compare to modulo: ~75% remap on same change

2. **Logarithmic lookup**: O(log(N × R)) where N=nodes, R=replicas (default R=10)
   - 10 nodes × 10 replicas = 100 entries → ~7 comparisons (binary search)
   - Acceptable performance even at scale

3. **Improved distribution**: Virtual nodes smooth out hash distribution
   - Without replicas: some nodes might get 2x traffic of others (hash clustering)
   - With 10 replicas: distribution within ~10% of ideal

4. **Industry standard**: Consistent hashing is well-understood, documented, proven
   - Used by Cassandra, Riak, DynamoDB, memcached
   - Extensive literature and debugging tools available

5. **Predictable**: Easy to reason about where a key maps (deterministic hash + sorted ring)

### Negative

1. **More complex than modulo**: Requires data structure (sorted ring), binary search
   - Trade-off: Complexity pays for minimal redistribution
   - Complexity is encapsulated in `get-node-for-key` function

2. **Memory overhead**: Hash ring stores N × R entries (default: 10× nodes)
   - For 100 nodes: 1000 ring entries × ~16 bytes each = ~16 KB
   - Negligible for most use cases

3. **Not perfect distribution**: Still possible for keys to cluster on certain nodes
   - Mitigated by increasing replicas (default 10 is reasonable trade-off)
   - Perfect distribution would require per-key state (defeats purpose of hashing)

4. **Ring rebuild on topology change**: Adding/removing node requires sorting
   - O(N log N) where N = ring size
   - Acceptable since topology changes are infrequent
   - Pool is locked during rebuild (brief disruption to concurrent operations)

### Implementation Notes

**Virtual nodes (replicas)**:
- Each physical node creates R virtual nodes with different hash values
- Virtual node hash: `(sxhash (format nil "~A-~D" node-name replica-index))`
- Example: "node1" creates "node1-0", "node1-1", ..., "node1-9"

**Why virtual nodes matter**:
```
Without replicas (1 per node):
  3 nodes → 3 ring positions → uneven distribution
  Node A: 45% of keys
  Node B: 30% of keys
  Node C: 25% of keys

With 10 replicas:
  3 nodes → 30 ring positions → smoother distribution
  Node A: 34% of keys
  Node B: 33% of keys
  Node C: 33% of keys
```

**Hash function choice**:
- Uses `sxhash` (Common Lisp built-in string hash)
- Portable across CL implementations
- Not cryptographically secure (doesn't need to be)
- Code comment acknowledges this is "simple" and can be replaced if needed
- For production at scale, could use MurmurHash3 or similar

## Related Decisions

- **ADR-0004**: Node Removal Limitation (explains why items aren't migrated)

## Related Contracts

- `make-distributed-pool` - Creates hash ring with replicas
- `fetch-from-distributed-pool` - Uses consistent hashing for key lookup
- `return-to-distributed-pool` - Same key must return to same node
- `add-node-to-distributed-pool` - Inserts virtual nodes, rebalances ring
- `remove-node-from-distributed-pool` - Removes virtual nodes, rebalances ring
- `get-node-for-key` - Exposes key-to-node mapping

## Notes

### Commits

- cead997 (2025-06-09): Initial distributed pool implementation
- 5a964ce: Added tests for distributed pool
- bbcff79: Minor fixes to distributed pool
- 8750268: Merged distributed-pool-consistent-hashing branch

### Contributors

- google-labs-jules[bot]: AI agent implementation

### Code Locations

- Hash ring struct: pooler.lisp:130-133
- Key hashing: pooler.lisp:134-136 (`internal-string-hash`)
- Ring search: pooler.lisp:137-154 (in `get-node-for-key`)
- Node addition: pooler.lisp:181-213 (`add-node-to-distributed-pool`)
- Node removal: pooler.lisp:215-236 (`remove-node-from-distributed-pool`)

### Test Coverage

Tests in `tests/test-pooler.lisp`:
- Basic distributed pool creation
- Key-to-node mapping consistency
- Node addition and removal
- Concurrent operations

### Future Considerations

- Allow custom hash functions (not just `sxhash`)
- Configurable replica count per node (currently global setting)
- Weighted nodes (some nodes get more replicas for higher capacity)
- Monitoring: distribution metrics, key redistribution counts
- Replace `sxhash` with more robust hash (MurmurHash3, CityHash)

### References

- Original paper: Karger et al. "Consistent Hashing and Random Trees" (1997)
- Industry usage: Cassandra, Riak, Amazon DynamoDB
- Blog post: "A Guide to Consistent Hashing" (Tom White, 2007)
