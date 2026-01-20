---
type: property
feature: distributed-pooling
category: concurrency_and_errors
status: draft
confidence: 0.85
---

# Concurrency and Error Properties

Properties governing thread safety, error conditions, and concurrent operations.

## CONCURRENCY-DP-001: Distributed Pool Lock Protects Hash Ring

**Property**: All hash ring modifications are protected by distributed pool lock

**Formal Statement**:
```
∀ dist-pool, ∀ operation ∈ {add-node, remove-node, get-node}:
  operation executes within (with-pool-lock dist-pool.lock ...)
```

**Operations Protected**:
- `add-node-to-distributed-pool`: Modifies hash ring (add virtual nodes, re-sort)
- `remove-node-from-distributed-pool`: Modifies hash ring (remove virtual nodes)
- `get-node-for-key`: Reads hash ring (binary search)
- `fetch-from-distributed-pool`: Reads hash ring (calls get-node-for-key)
- `return-to-distributed-pool`: Reads hash ring (calls get-node-for-key)

**Evidence**:
- Code: pooler.lisp:143 (get-node-for-key)
- Code: pooler.lisp:186 (add-node-to-distributed-pool)
- Code: pooler.lisp:221 (remove-node-from-distributed-pool)

**Guarantee**: No race conditions on hash ring structure.

**Confidence**: 1.0

---

## CONCURRENCY-DP-002: Individual Node Pools Have Separate Locks

**Property**: Each node's pool has independent lock (not shared with distributed pool lock)

**Formal Statement**:
```
∀ dist-pool, ∀ node1, node2 ∈ dist-pool.nodes:
  node1.lock ≠ node2.lock
  node1.lock ≠ dist-pool.lock
```

**Benefit**: Fetches on different nodes can proceed concurrently (not serialized).

**Lock Hierarchy**:
1. Distributed pool lock: Hash ring + node list
2. Node pool lock: Queue + counters (per node)

**Evidence**:
- Code: Each pool created with own lock (make-pool creates lock)
- Design: Lock-per-node for concurrency

**Example**:
```lisp
;; Thread 1: fetch from node1 (holds node1.lock)
;; Thread 2: fetch from node2 (holds node2.lock)
;; → Can proceed concurrently (different locks)

;; Thread 1: add-node (holds dist-pool.lock)
;; Thread 2: fetch (needs dist-pool.lock for hash ring lookup)
;; → Thread 2 waits (same lock)
```

**Confidence**: 0.95

---

## CONCURRENCY-DP-003: Topology Changes are Serialized

**Property**: Node additions and removals are serialized (one at a time)

**Formal Statement**:
```
∀ dist-pool, ∀ t1, t2 (concurrent threads):
  t1: add-node(nodeA)
  t2: add-node(nodeB)
  ⇒ one completes before other starts (serialized via lock)
```

**Consequence**: Topology changes are not parallelized.

**Rationale**: Hash ring rebuild (sort) requires exclusive access.

**Evidence**:
- Code: Both operations hold distributed pool lock for entire operation
- Scenario: 04-concurrency-and-errors.md Scenario 8

**Performance Impact**: Multiple concurrent add-node calls will execute sequentially.

**Confidence**: 0.9

---

## CONCURRENCY-DP-004: Fetch Blocks on Topology Change

**Property**: Fetch operations wait if topology change in progress

**Formal Statement**:
```
∀ dist-pool:
  Thread 1: add-node (holds dist-pool.lock)
  Thread 2: fetch-from-distributed-pool (needs dist-pool.lock for hash lookup)
  ⇒ Thread 2 blocks until Thread 1 completes
```

**Brief Blocking**: Topology changes are fast (typically milliseconds).

**Evidence**:
- Code: fetch-from-distributed-pool → get-node-for-key → with-pool-lock
- Scenario: 04-concurrency-and-errors.md Scenario 2

**Mitigation**: Add/remove nodes during low-traffic periods if possible.

**Confidence**: 0.85

---

## ERROR-DP-001: Empty Hash Ring Signals pool-error

**Property**: Fetching from distributed pool with no nodes signals pool-error

**Formal Statement**:
```
∀ dist-pool:
  len(dist-pool.hash-ring) = 0
  ⇒ fetch-from-distributed-pool signals pool-error
```

**Error Message**: "Hash ring is empty" or "No nodes available"

**Trigger**: All nodes removed, or created with empty node-configs.

**Evidence**:
- Code: pooler.lisp:101-102 (get-node-id-for-key-from-ring errors if ring is empty)
- Tests: tests/test-pooler.lisp:134-138
- Contract: fetch-from-distributed-pool.md (error conditions)

**Recovery**: Add at least one node before fetching.

**Confidence**: 1.0

---

## ERROR-DP-002: Missing item-maker Signals pool-error

**Property**: Creating distributed pool without item-maker signals pool-error

**Formal Statement**:
```
∀ make-distributed-pool call:
  (no global :item-maker) ∧ (∃ node-config without :item-maker)
  ⇒ signals pool-error
```

**Required**: Either global `:item-maker` OR `:item-maker` in every node-config.

**Evidence**:
- Code: pooler.lisp:128-130 (pool-error if node-item-maker is nil)
- Tests: tests/test-pooler.lisp:125-133
- Contract: make-distributed-pool.md (error conditions)

**Example**:
```lisp
;; Error: No item-maker anywhere
(make-distributed-pool
  :node-configs '((:name "node1")))
; → pool-error

;; OK: Global item-maker
(make-distributed-pool
  :item-maker #'make-item
  :node-configs '((:name "node1")))
```

**Confidence**: 1.0

---

## ERROR-DP-003: Duplicate Node Name Signals pool-error

**Property**: Adding node with existing name signals pool-error

**Formal Statement**:
```
∀ dist-pool, ∀ node-name:
  (node-name ∈ dist-pool.nodes)
  ⇒ add-node(node-name) signals pool-error
```

**Validation**: Before adding node, check name doesn't exist.

**Evidence**:
- Contract: add-node-to-distributed-pool.md (error conditions)
- Scenario: 02-node-management.md Scenario 5

**Recovery**: Use unique node names.

**Confidence**: 0.9

---

## ERROR-DP-004: Remove Non-Existent Node Signals pool-error

**Property**: Removing node that doesn't exist signals pool-error

**Formal Statement**:
```
∀ dist-pool, ∀ node-name:
  (node-name ∉ dist-pool.nodes)
  ⇒ remove-node(node-name) signals pool-error
```

**Evidence**:
- Tests: tests/test-pooler.lisp:89-98
- Contract: remove-node-from-distributed-pool.md (error conditions)

**Confidence**: 1.0

---

## ERROR-DP-005: item-maker Failure Propagates

**Property**: If item-maker fails beyond retry limit, error propagates to caller

**Formal Statement**:
```
∀ dist-pool, ∀ key, ∀ tries:
  ∀ attempt ∈ [1..tries]:
    item-maker() signals error
  ⇒ fetch-from-distributed-pool signals pool-item-creation-error
```

**Retry**: Delegated to underlying pool's fetch-from (tries N times).

**Final Failure**: After all retries exhausted, error propagates.

**Evidence**:
- Contract: fetch-from.md (error conditions)
- Contract: fetch-from-distributed-pool.md (retry behavior)
- Scenario: 04-concurrency-and-errors.md Scenario 6, 7

**Confidence**: 0.85

---

## SAFETY-DP-001: No Data Races on Hash Ring

**Property**: Hash ring modifications and reads are race-free

**Formal Statement**:
```
∀ dist-pool, ∀ concurrent operations:
  no data race on hash-ring array
  (protected by mutex)
```

**Mechanism**: Lock-based synchronization (with-pool-lock).

**Evidence**:
- Code: All hash ring access within with-pool-lock
- Platform: Lock abstraction via utils.lisp (portable across CL implementations)

**Note**: Not lock-free (uses mutex), but race-free.

**Confidence**: 0.95

---

## SAFETY-DP-002: No Deadlocks in Normal Operation

**Property**: No circular lock dependencies in distributed pool operations

**Formal Statement**:
```
∀ dist-pool operations:
  no circular wait on locks
  (lock hierarchy: dist-pool.lock → node.lock)
```

**Lock Hierarchy**:
1. Distributed pool lock acquired first (if needed)
2. Node pool lock acquired second (if needed)
3. Locks always released in reverse order (LIFO via unwind-protect)

**No Circular Dependencies**:
- Distributed pool operations never acquire node locks while holding dist-pool lock
- Node operations don't acquire dist-pool lock

**Evidence**:
- Code: Lock acquisition order is consistent
- Design: Simple lock hierarchy

**Confidence**: 0.9

---

## SAFETY-DP-003: Return After Topology Change (Edge Case)

**Property**: Returning item after node removal may cause silent pool corruption

**Formal Statement**:
```
∀ dist-pool, ∀ key, ∀ node-old:
  item = fetch-from-distributed-pool(key)  ; key → node-old
  remove-node(node-old)
  node-new = get-node-for-key(key)  ; key → node-new (different)
  return-to-distributed-pool(key, item)
  ⇒ item returned to node-new (not node-old)
  ⇒ pool state anomaly (item from nodeA in nodeB)
```

**Risk**: Low (requires holding item reference across topology change).

**Mitigation**: Return items promptly, use short-lived fetches.

**Evidence**:
- Scenario: 04-concurrency-and-errors.md Scenario 9
- Logic: Topology change between fetch and return causes rerouting

**Not Prevented**: No validation that item came from correct node.

**Confidence**: 0.8

---

## Summary

### Concurrency Properties

| ID | Property | Mechanism | Confidence |
|----|----------|-----------|------------|
| CONCURRENCY-DP-001 | Hash ring lock protection | Mutex | 1.0 |
| CONCURRENCY-DP-002 | Separate node locks | Lock-per-node | 0.95 |
| CONCURRENCY-DP-003 | Serialized topology changes | Single lock | 0.9 |
| CONCURRENCY-DP-004 | Fetch blocks on topology change | Lock contention | 0.85 |

### Error Properties

| ID | Property | Condition | Confidence |
|----|----------|-----------|------------|
| ERROR-DP-001 | Empty ring error | No nodes | 1.0 |
| ERROR-DP-002 | Missing item-maker error | Invalid config | 1.0 |
| ERROR-DP-003 | Duplicate name error | Add existing name | 0.9 |
| ERROR-DP-004 | Node not found error | Remove non-existent | 1.0 |
| ERROR-DP-005 | Creation failure propagation | item-maker fails | 0.85 |

### Safety Properties

| ID | Property | Guarantee | Confidence |
|----|----------|-----------|------------|
| SAFETY-DP-001 | No hash ring races | Lock-based sync | 0.95 |
| SAFETY-DP-002 | No deadlocks | Lock hierarchy | 0.9 |
| SAFETY-DP-003 | Return after removal (edge case) | Known issue | 0.8 |

**Key Insights**:
1. Thread safety via lock-based synchronization (not lock-free)
2. Topology changes serialize all operations briefly
3. Separate locks per node enable concurrent fetches on different nodes
4. Error conditions are well-defined and testable

**Related**:
- invariants.md: Structural guarantees
- behavioral-properties.md: Routing and lifecycle properties
- Scenarios: 04-concurrency-and-errors.md
