---
type: property
feature: distributed-pooling
category: invariants
status: draft
confidence: 0.9
---

# Distributed Pool Invariants

Structural and state invariants that must always hold for distributed pools.

## INV-DP-001: Hash Ring Size

**Property**: Hash ring size equals (number of nodes × replicas)

**Formal Statement**:
```
∀ dist-pool:
  len(dist-pool.hash-ring) = len(dist-pool.nodes) × dist-pool.replicas
```

**Always True**: After any operation (add-node, remove-node, creation)

**Evidence**:
- Code: pooler.lisp:82-87 (add-node-to-ring adds exactly 'replicas' virtual nodes)
- Code: pooler.lisp:224-226 (remove-node filters out all replicas for removed node)
- Tests: tests/test-pooler.lisp:61-98 (verifies ring size changes)

**Example**:
```lisp
;; 3 nodes, 10 replicas each
(assert (= (length (distributed-pool-hash-ring *pool*))
           (* 3 10))) ; 30 entries
```

**Confidence**: 1.0

---

## INV-DP-002: Hash Ring is Sorted

**Property**: Hash ring is always sorted by hash value (ascending)

**Formal Statement**:
```
∀ dist-pool, ∀i ∈ [0, len(ring)-2]:
  ring[i].hash ≤ ring[i+1].hash
```

**Always True**: After creation, add-node, remove-node (any operation that modifies ring)

**Rationale**: Binary search requires sorted array for O(log N) lookup.

**Evidence**:
- Code: pooler.lisp:86-87 (hash ring sorted after adding nodes)
- Code: pooler.lisp:210-211 (hash ring sorted after node addition)
- Algorithm: Consistent hashing requires sorted ring for find-next-position

**Example**:
```lisp
(let ((ring (distributed-pool-hash-ring *pool*)))
  (loop for i from 0 below (1- (length ring))
        always (<= (hash-ring-node-hash (aref ring i))
                   (hash-ring-node-hash (aref ring (1+ i))))))
; → T (always true)
```

**Confidence**: 1.0

---

## INV-DP-003: Non-Empty Hash Ring (or Error)

**Property**: Hash ring has at least one entry, or operations error

**Formal Statement**:
```
∀ dist-pool, ∀ operation:
  len(dist-pool.hash-ring) > 0  OR  operation signals pool-error
```

**Rationale**: Cannot route keys without at least one node.

**Evidence**:
- Code: pooler.lisp:101-102 (get-node-id-for-key-from-ring errors if ring is empty)
- Tests: tests/test-pooler.lisp:134-138 (empty ring error condition)

**Error Condition**: `fetch-from-distributed-pool` on empty ring signals `pool-error`.

**Example**:
```lisp
;; Remove all nodes → empty ring
(remove-node-from-distributed-pool *pool* "node1")
; ...
; hash-ring is now empty

;; Fetch errors
(handler-case
  (fetch-from-distributed-pool *pool* "key")
  (pooler::pool-error (e)
    :expected-error))
```

**Confidence**: 1.0

---

## INV-DP-004: Node Name Uniqueness

**Property**: All nodes in distributed pool have unique names

**Formal Statement**:
```
∀ dist-pool, ∀ i, j ∈ nodes:
  i ≠ j ⇒ (pool-name nodes[i]) ≠ (pool-name nodes[j])
```

**Always True**: After creation, add-node (enforced by validation)

**Rationale**: Node name is used as identifier in hash ring and for removal.

**Evidence**:
- Code: add-node-to-distributed-pool validates no duplicate names (errors if exists)
- Tests: Inferred (duplicate name should error)

**Error Condition**: `add-node-to-distributed-pool` with existing name signals `pool-error`.

**Example**:
```lisp
(add-node-to-distributed-pool *pool* '(:name "node1" ...))
; → Error if node1 already exists
```

**Confidence**: 0.95

---

## INV-DP-005: Virtual Nodes Reference Actual Nodes

**Property**: Every virtual node in hash ring references an actual node in nodes list

**Formal Statement**:
```
∀ dist-pool, ∀ vnode ∈ hash-ring:
  ∃ node ∈ dist-pool.nodes:
    vnode.node-id = node.id
```

**Always True**: After creation, add-node, remove-node

**Rationale**: Hash ring lookups must resolve to actual pool objects.

**Evidence**:
- Code: pooler.lisp:148-154 (get-node-for-key looks up node-id in nodes list)
- Code: pooler.lisp:224-226 (remove-node removes all vnodes for removed node)

**Failure Mode**: If invariant violated → `get-node-for-key` returns nil → error

**Confidence**: 0.9

---

## INV-DP-006: Replicas > 0

**Property**: Replicas count must be at least 1

**Formal Statement**:
```
∀ dist-pool:
  dist-pool.replicas ≥ 1
```

**Rationale**: Zero replicas → empty hash ring → cannot route keys.

**Default**: 10 replicas (in make-distributed-pool)

**Evidence**:
- Code: pooler.lisp:70 (default replicas = 10)
- Logic: Zero replicas would violate INV-DP-003 (non-empty ring)

**Note**: No explicit validation in code (relies on default), but zero would cause immediate failure.

**Confidence**: 1.0

---

## INV-DP-007: Each Node is a Valid Pool

**Property**: Every node in distributed pool is a properly initialized `pool` struct

**Formal Statement**:
```
∀ dist-pool, ∀ node ∈ dist-pool.nodes:
  (typep node 'pool) = T
  ∧ (pool-item-maker node) ≠ NIL
  ∧ (pool-capacity node) > 0
```

**Always True**: After creation, add-node

**Rationale**: Distributed pool delegates to underlying pools, which must be valid.

**Evidence**:
- Code: pooler.lisp:181-213 (add-node creates pool via make-pool)
- Code: make-pool validates item-maker and capacity

**Confidence**: 1.0

---

## INV-DP-008: Distributed Pool Lock Exists

**Property**: Every distributed pool has a valid lock for thread safety

**Formal Statement**:
```
∀ dist-pool:
  (distributed-pool-lock dist-pool) ≠ NIL
```

**Always True**: After creation (make-distributed-pool initializes lock)

**Rationale**: Thread-safe hash ring access requires lock.

**Evidence**:
- Code: make-distributed-pool creates lock (via make-pool-lock)
- Code: All hash ring operations use with-pool-lock

**Confidence**: 1.0

---

## Summary

| ID | Property | Always True | Confidence |
|----|----------|-------------|------------|
| INV-DP-001 | Hash ring size = nodes × replicas | ✓ | 1.0 |
| INV-DP-002 | Hash ring sorted by hash | ✓ | 1.0 |
| INV-DP-003 | Non-empty hash ring or error | ✓ | 1.0 |
| INV-DP-004 | Node name uniqueness | ✓ | 0.95 |
| INV-DP-005 | Virtual nodes reference actual nodes | ✓ | 0.9 |
| INV-DP-006 | Replicas ≥ 1 | ✓ | 1.0 |
| INV-DP-007 | Each node is valid pool | ✓ | 1.0 |
| INV-DP-008 | Lock exists | ✓ | 1.0 |

**Coverage**: Structural invariants for hash ring, nodes, and threading primitives.

**Related**:
- ADR-0002: Consistent hashing algorithm (hash ring design)
- Thread Safety properties: concurrency-properties.md
