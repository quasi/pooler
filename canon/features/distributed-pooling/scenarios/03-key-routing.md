---
type: scenario
feature: distributed-pooling
category: key_routing
status: draft
confidence: 0.85
---

# Key Routing and Consistency Scenarios

Scenarios demonstrating consistent hashing behavior, key distribution, and routing guarantees.

## Scenario 1: Same Key Always Maps to Same Node

**Given**:
- Distributed pool with 3 nodes
- No topology changes

**When**:
```lisp
(let ((node1 (get-node-for-key *dist-pool* "user:alice"))
      (node2 (get-node-for-key *dist-pool* "user:alice"))
      (node3 (get-node-for-key *dist-pool* "user:alice")))
  (list node1 node2 node3))
```

**Then**:
- All three calls return same node object (identity equal)
- `(eq node1 node2)` = T
- `(eq node2 node3)` = T
- Deterministic routing (hash value for key is stable)

**Property**: Deterministic mapping (from get-node-for-key contract)

**Source**: get-node-for-key contract, tests/test-pooler.lisp:100-123
**Confidence**: 1.0

---

## Scenario 2: Different Keys Distribute Across Nodes

**Given**:
- Distributed pool with 2 nodes (node1, node2)
- replicas = 20 (high replica count for good distribution)

**When**:
```lisp
(let ((node-counts (make-hash-table :test 'equal)))
  (dotimes (i 50)
    (let* ((key (format nil "key-~A" i))
           (node (get-node-for-key *dist-pool* key))
           (name (pool-name node)))
      (incf (gethash name node-counts 0))))
  node-counts)
```

**Then**:
- Both nodes receive some keys (neither count is 0)
- Distribution is non-uniform but reasonable (e.g., 22 vs 28, not 50 vs 0)
- `(gethash "node1" node-counts)` > 0
- `(gethash "node2" node-counts)` > 0
- Sum of counts = 50 (all keys mapped)

**Property**: Keys distribute across nodes (non-zero distribution)

**Source**: tests/test-pooler.lisp:100-123
**Confidence**: 0.9

---

## Scenario 3: Distribution Uniformity with Virtual Nodes

**Given**:
- Distributed pool with 3 nodes
- Test with replicas = 1 vs replicas = 20

**When**:
```lisp
;; Pool with 1 replica per node
(defvar *pool-low-replicas*
  (make-distributed-pool
    :node-configs '((:name "n1") (:name "n2") (:name "n3"))
    :replicas 1
    :item-maker #'make-item))

;; Pool with 20 replicas per node
(defvar *pool-high-replicas*
  (make-distributed-pool
    :node-configs '((:name "n1") (:name "n2") (:name "n3"))
    :replicas 20
    :item-maker #'make-item))

;; Distribute 300 keys
(defun measure-distribution (pool)
  (let ((counts (make-hash-table :test 'equal)))
    (dotimes (i 300)
      (let* ((key (format nil "key-~A" i))
             (node (get-node-for-key pool key)))
        (incf (gethash (pool-name node) counts 0))))
    counts))
```

**Then**:
- Low replicas: Uneven distribution (e.g., n1: 45%, n2: 30%, n3: 25%)
- High replicas: Better distribution (e.g., n1: 34%, n2: 33%, n3: 33%)
- Standard deviation lower with more replicas
- Ideal distribution: 100 keys per node (33.3% each)

**Property**: More virtual nodes → better distribution uniformity

**Source**: ADR-0002 (consistent hashing rationale), inferred from design
**Confidence**: 0.85

---

## Scenario 4: Key Hashing is Deterministic

**Given**:
- Distributed pool with nodes

**When**:
```lisp
(let ((hash1 (pooler::internal-string-hash "user:alice"))
      (hash2 (pooler::internal-string-hash "user:alice")))
  (list hash1 hash2))
```

**Then**:
- `hash1 = hash2` (same input → same hash)
- Hash is integer (fixnum)
- Hash is based on `sxhash` (Common Lisp built-in)

**Note**: `internal-string-hash` is internal function (not exported)

**Source**: get-node-for-key contract (implementation details), ADR-0002
**Confidence**: 1.0

---

## Scenario 5: Fetch and Return Use Same Node

**Given**:
- Distributed pool with nodes
- Key "user:alice"

**When**:
```lisp
(let ((item (fetch-from-distributed-pool *dist-pool* "user:alice"))
      (node-before (get-node-for-key *dist-pool* "user:alice")))
  ;; Use item
  (return-to-distributed-pool *dist-pool* "user:alice" item)
  (let ((node-after (get-node-for-key *dist-pool* "user:alice")))
    (eq node-before node-after)))
```

**Then**:
- `node-before` and `node-after` are same object (eq)
- Item fetched from nodeX and returned to nodeX (routing consistency)
- No topology changes occurred during fetch/return cycle

**Property**: Routing consistency (fetch and return use same node for same key)

**Source**: return-to-distributed-pool contract (routing consistency property)
**Confidence**: 0.95

---

## Scenario 6: Inspect Key Distribution Across Nodes

**Given**:
- Distributed pool with 3 nodes
- List of keys representing real usage pattern

**When**:
```lisp
(defun analyze-distribution (dist-pool keys)
  (let ((node-counts (make-hash-table :test 'equal)))
    (dolist (key keys)
      (let* ((node (get-node-for-key dist-pool key))
             (name (pool-name node)))
        (incf (gethash name node-counts 0))))
    ;; Return counts
    (maphash (lambda (name count)
               (format t "~A: ~A keys (~,1F%)~%"
                       name count
                       (* 100.0 (/ count (length keys)))))
             node-counts)
    node-counts))

(analyze-distribution
  *dist-pool*
  '("user:alice" "user:bob" "session:123" "session:456"
    "resource:db1" "resource:cache" "order:1001" "order:1002"))
```

**Then**:
- Output shows key count and percentage per node
- Useful for debugging key distribution
- Can identify hot nodes (nodes with disproportionate keys)

**Example Output**:
```
node1: 3 keys (37.5%)
node2: 2 keys (25.0%)
node3: 3 keys (37.5%)
```

**Source**: get-node-for-key contract (usage examples - debugging distribution)
**Confidence**: 0.8

---

## Scenario 7: Pre-warming Specific Node for Key

**Given**:
- Distributed pool with nodes
- Key "important-user" known to have high traffic

**When**:
```lisp
(defun prewarm-for-key (dist-pool key count)
  (let ((node (get-node-for-key dist-pool key)))
    (grow-pool node count)))

;; Pre-warm node that will handle "important-user"
(prewarm-for-key *dist-pool* "important-user" 10)
```

**Then**:
- Node responsible for "important-user" has 10 pre-created items
- First 10 fetches for "important-user" don't trigger item creation
- Other nodes unaffected
- Reduces latency for known hot keys

**Use Case**: Anticipating traffic for specific keys (VIP users, scheduled jobs)

**Source**: get-node-for-key contract (usage examples - pre-warming)
**Confidence**: 0.8

---

## Scenario 8: Key Redistribution After Adding Node

**Given**:
- Distributed pool with 3 nodes (n1, n2, n3)
- 90 keys evenly distributed (30 per node)
- replicas = 10 (default)

**When**:
- Add 4th node (n4)
- Recalculate distribution

**Then**:
- Approximately 1/4 of keys reroute to n4 (~22-23 keys)
- Each original node loses ~7-8 keys to n4
- Final distribution: ~22-23 keys per node (4 nodes)
- NOT all keys reroute (only ~25%, not 100%)

**Math** (from ADR-0002):
- Adding node N to N-1 nodes: ~1/N keys remap
- 3 → 4 nodes: ~25% of keys remap (22.5 out of 90)
- Compare to modulo hashing: ~75% would remap

**Property**: Minimal redistribution (consistent hashing advantage)

**Source**: ADR-0002 (consistent hashing rationale), add-node-to-distributed-pool contract
**Confidence**: 0.85

---

## Scenario 9: Key Redistribution After Removing Node

**Given**:
- Distributed pool with 4 nodes (n1, n2, n3, n4)
- Keys distributed across all 4 nodes

**When**:
- Remove n4
- Keys that mapped to n4 now reroute

**Then**:
- All keys previously on n4 reroute to n1, n2, or n3
- Keys on n1, n2, n3 stay on their original nodes (don't reroute)
- Only keys from removed node redistribute (not entire keyspace)
- Redistribution is deterministic (same keys always go to same new nodes)

**Example**:
```
Before removal:
  n1: 22 keys, n2: 23 keys, n3: 22 keys, n4: 23 keys

After removing n4:
  n1: 30 keys (+8 from n4)
  n2: 30 keys (+7 from n4)
  n3: 30 keys (+8 from n4)
  n4: (removed)
```

**Property**: Removed node's keys redistribute, others unchanged

**Source**: ADR-0002, remove-node-from-distributed-pool contract
**Confidence**: 0.85

---

## Scenario 10: Custom Routing Logic Built on get-node-for-key

**Given**:
- Distributed pool with nodes
- Need to fetch from primary key, fallback to secondary key if primary node is empty

**When**:
```lisp
(defun fetch-with-fallback (dist-pool primary-key fallback-key)
  (let ((primary-node (get-node-for-key dist-pool primary-key)))
    (if (> (pool-current-size primary-node) 0)
        (fetch-from primary-node)
        ;; Fallback: use different node
        (let ((fallback-node (get-node-for-key dist-pool fallback-key)))
          (fetch-from fallback-node)))))
```

**Then**:
- If primary node has items → fetch from primary
- If primary node empty → fetch from fallback node
- Custom routing logic implemented on top of standard API
- Bypasses distributed pool abstraction (advanced usage)

**Caution**: Bypassing abstraction requires careful key management for returns.

**Source**: get-node-for-key contract (usage examples - custom routing)
**Confidence**: 0.75

---

## Summary

These scenarios cover:
- ✓ Deterministic key-to-node mapping
- ✓ Key distribution across nodes
- ✓ Impact of virtual nodes (replicas) on distribution
- ✓ Routing consistency (fetch and return)
- ✓ Key redistribution on topology changes
- ✓ Distribution analysis and debugging
- ✓ Advanced patterns (pre-warming, custom routing)

**Key Properties**:
1. Same key → same node (always, if topology unchanged)
2. Different keys → distributed across nodes
3. More replicas → better distribution uniformity
4. Topology changes → partial redistribution (minimal)
5. Removed node's keys → redistribute to remaining nodes
6. Consistent hashing → O(log N) lookup performance

**Related**:
- ADR-0002: Consistent hashing algorithm details
- get-node-for-key contract: Advanced API for routing inspection
