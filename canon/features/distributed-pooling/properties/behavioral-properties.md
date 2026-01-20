---
type: property
feature: distributed-pooling
category: behavioral_properties
status: draft
confidence: 0.85
---

# Distributed Pool Behavioral Properties

Properties governing distributed pool behavior, routing, and lifecycle.

## BEHAVIOR-DP-001: Deterministic Key-to-Node Mapping

**Property**: Same key always maps to same node (if topology unchanged)

**Formal Statement**:
```
∀ dist-pool, ∀ key, ∀ t1, t2:
  topology_unchanged(t1, t2)
  ⇒ get-node-for-key(key, t1) = get-node-for-key(key, t2)
```

**Deterministic**: Hash function (sxhash) is deterministic, binary search is deterministic.

**Topology Unchanged**: Same nodes, same hash ring (no add-node/remove-node between t1 and t2).

**Evidence**:
- Code: pooler.lisp:97-109 (deterministic hash-based lookup)
- Code: internal-string-hash uses sxhash (deterministic)
- Tests: tests/test-pooler.lisp:38-52 (repeated fetches use same key)
- Contract: get-node-for-key.md (Deterministic Mapping property)

**Example**:
```lisp
(let ((node1 (get-node-for-key *pool* "user:alice"))
      (node2 (get-node-for-key *pool* "user:alice")))
  (assert (eq node1 node2))) ; Same object
```

**Consequence**: Session affinity, sharding work correctly.

**Confidence**: 1.0

---

## BEHAVIOR-DP-002: Fetch-Return Routing Consistency

**Property**: Item fetched with key K must be returned with key K to same node

**Formal Statement**:
```
∀ dist-pool, ∀ key:
  node_fetch = get-node(fetch-from-distributed-pool(key))
  node_return = get-node(return-to-distributed-pool(key))
  ⇒ node_fetch = node_return  (if topology unchanged)
```

**Critical**: Using different keys for fetch vs return corrupts pool state.

**Evidence**:
- Contract: return-to-distributed-pool.md (Routing Consistency property)
- Contract: return-to-distributed-pool.md (anti-pattern: wrong key on return)
- Code: Both use get-node-for-key internally → same deterministic routing

**Anti-Pattern**:
```lisp
(let ((item (fetch-from-distributed-pool *pool* "user:alice")))
  ;; WRONG: Different key on return
  (return-to-distributed-pool *pool* "user:bob" item))
; Item from nodeA returned to nodeB → corruption
```

**Correct Pattern**:
```lisp
(let ((key "user:alice"))
  (let ((item (fetch-from-distributed-pool *pool* key)))
    (unwind-protect
      (use item)
      (return-to-distributed-pool *pool* key item))))
```

**Confidence**: 1.0

---

## BEHAVIOR-DP-003: Key Distribution Across Nodes

**Property**: Keys distribute non-uniformly but reasonably across nodes

**Formal Statement**:
```
∀ dist-pool, ∀ key-set (sufficiently large):
  ∀ node ∈ dist-pool.nodes:
    count(keys mapped to node) > 0
```

**Non-Zero**: Every node receives some keys (unless zero replicas, which is invalid).

**Uniformity**: Depends on replica count (more replicas → better uniformity).

**Evidence**:
- Tests: tests/test-pooler.lisp:100-123 (50 keys distribute across 2 nodes)
- Contract: get-node-for-key.md (example: analyze-distribution)
- ADR-0002: Virtual nodes improve distribution uniformity

**Example Distribution** (3 nodes, 10 replicas, 90 keys):
```
node1: 28 keys (31%)
node2: 32 keys (36%)
node3: 30 keys (33%)
```

**Not Guaranteed**: Perfect uniformity (33.3% each) due to hash clustering.

**Confidence**: 0.9

---

## BEHAVIOR-DP-004: Minimal Key Redistribution on Node Addition

**Property**: Adding node N causes approximately 1/N of keys to reroute

**Formal Statement**:
```
∀ dist-pool with (N-1) nodes:
  add-node(nodeN)
  ⇒ ~(1/N) of keys reroute to nodeN
  ⇒ ~((N-1)/N) of keys stay on original nodes
```

**Example**:
- 3 → 4 nodes: ~25% of keys reroute
- 9 → 10 nodes: ~10% of keys reroute

**Contrast**: Simple modulo hashing: ~75% reroute (3→4 nodes).

**Evidence**:
- ADR-0002: Consistent hashing rationale (minimal redistribution)
- Contract: add-node-to-distributed-pool.md (key redistribution section)
- Scenario: 03-key-routing.md Scenario 8

**Confidence**: 0.85

---

## BEHAVIOR-DP-005: Complete Key Redistribution on Node Removal

**Property**: Removing node causes all keys on that node to reroute, others stay

**Formal Statement**:
```
∀ dist-pool, ∀ nodeX:
  keys_on_X = {key | get-node-for-key(key) = nodeX}
  remove-node(nodeX)
  ⇒ ∀ key ∈ keys_on_X: get-node-for-key(key) ≠ nodeX
  ⇒ ∀ key ∉ keys_on_X: get-node-for-key(key) unchanged
```

**Only Affected Keys Reroute**: Keys not on removed node stay on original nodes.

**Deterministic Redistribution**: Keys from removed node go to specific other nodes (not random).

**Evidence**:
- ADR-0002: Consistent hashing behavior
- Contract: remove-node-from-distributed-pool.md (key redistribution)
- Scenario: 03-key-routing.md Scenario 9

**Confidence**: 0.85

---

## BEHAVIOR-DP-006: Node Removal Orphans Items

**Property**: Removing node does NOT destroy or migrate items in its pool

**Formal Statement**:
```
∀ dist-pool, ∀ nodeX:
  items_before = count(items in nodeX.pool)
  remove-node(nodeX)
  ⇒ items in nodeX.pool unchanged (orphaned)
  ⇒ items NOT destroyed
  ⇒ items NOT migrated to other nodes
```

**Known Limitation**: Items are orphaned until manual cleanup.

**Evidence**:
- Code: pooler.lisp:229-231 (comment documents limitation)
- Contract: remove-node-from-distributed-pool.md (Limitation section)
- ADR-0004: Node Removal Without Item Migration (complete rationale)

**Workaround**: Manual drain before removal.

**Confidence**: 1.0

---

## BEHAVIOR-DP-007: Auto-Growth on Empty Node

**Property**: Fetching from distributed pool auto-grows underlying node if empty

**Formal Statement**:
```
∀ dist-pool, ∀ key:
  node = get-node-for-key(key)
  node.current-size = 0
  ⇒ fetch-from-distributed-pool(key) triggers grow-pool(node, threshold)
```

**Delegated**: Auto-growth is delegated to underlying pool's fetch-from behavior.

**Evidence**:
- Code: fetch-from-distributed-pool calls fetch-from on node
- Contract: fetch-from.md (auto-growth behavior)
- Scenario: 01-basic-operations.md Scenario 2

**Confidence**: 0.95

---

## BEHAVIOR-DP-008: Retry Logic Delegated to Node

**Property**: Distributed pool fetch retries are delegated to underlying node pool

**Formal Statement**:
```
∀ dist-pool, ∀ key, ∀ tries:
  fetch-from-distributed-pool(key, :tries N)
  = fetch-from(get-node-for-key(key), :tries N)
```

**Pass-Through**: `:tries` parameter passed directly to underlying pool.

**Evidence**:
- Code: pooler.lisp:156-167 (fetch-from-distributed-pool delegates to fetch-from)
- Contract: fetch-from-distributed-pool.md (:tries parameter)

**Confidence**: 1.0

---

## BEHAVIOR-DP-009: Timeout Checking per Node

**Property**: Each node has independent timeout checking (not global)

**Formal Statement**:
```
∀ dist-pool:
  ∀ node1, node2 ∈ dist-pool.nodes:
    node1.last-access independent of node2.last-access
    node1 timeout trigger ≠ node2 timeout trigger
```

**Node-Specific**: Timeout is checked per underlying pool, not distributed pool-wide.

**Example**:
- node1: Idle for 10 minutes → times out on next fetch
- node2: Active → doesn't timeout

**Evidence**:
- Code: Each node is separate `pool` struct with own `last-access`
- Contract: fetch-from.md (timeout checking)
- ADR-0001: Pool timeout with lazy checking

**Confidence**: 0.9

---

## BEHAVIOR-DP-010: Thread-Safe Hash Ring Access

**Property**: Hash ring access is serialized via distributed pool lock

**Formal Statement**:
```
∀ dist-pool, ∀ operations O1, O2:
  O1, O2 ∈ {fetch, add-node, remove-node, get-node-for-key}
  concurrent(O1, O2)
  ⇒ with-pool-lock serializes access
```

**Lock Scope**: Distributed pool lock protects hash ring and nodes list.

**Node Locks**: Each node has separate lock for its own pool operations.

**Evidence**:
- Code: pooler.lisp:143 (get-node-for-key uses with-pool-lock)
- Code: pooler.lisp:186 (add-node-to-distributed-pool uses with-pool-lock)
- Code: pooler.lisp:221 (remove-node-from-distributed-pool uses with-pool-lock)

**Consequence**: Operations are thread-safe but serialized (one at a time).

**Confidence**: 0.9

---

## Summary

| ID | Property | Type | Confidence |
|----|----------|------|------------|
| BEHAVIOR-DP-001 | Deterministic key mapping | Correctness | 1.0 |
| BEHAVIOR-DP-002 | Fetch-return consistency | Correctness | 1.0 |
| BEHAVIOR-DP-003 | Key distribution | Distribution | 0.9 |
| BEHAVIOR-DP-004 | Minimal redistribution (add) | Performance | 0.85 |
| BEHAVIOR-DP-005 | Complete redistribution (remove) | Correctness | 0.85 |
| BEHAVIOR-DP-006 | Orphaned items on removal | Limitation | 1.0 |
| BEHAVIOR-DP-007 | Auto-growth | Lifecycle | 0.95 |
| BEHAVIOR-DP-008 | Retry delegation | Delegation | 1.0 |
| BEHAVIOR-DP-009 | Per-node timeout | Lifecycle | 0.9 |
| BEHAVIOR-DP-010 | Thread-safe ring access | Concurrency | 0.9 |

**Key Insights**:
1. Deterministic routing enables sharding and session affinity
2. Minimal redistribution (consistent hashing) is major advantage
3. Node removal limitation is documented trade-off (simplicity vs automation)
4. Thread safety via lock serialization (not lock-free)

**Related**:
- ADR-0002: Consistent hashing (routing algorithm)
- ADR-0004: Node removal limitation (orphaned items)
- invariants.md: Structural invariants
- concurrency-properties.md: Thread safety details
