---
type: scenario
feature: distributed-pooling
category: node_management
status: draft
confidence: 0.85
---

# Node Management Scenarios

Scenarios covering dynamic node addition and removal from distributed pools.

## Scenario 1: Add Node to Running Distributed Pool

**Given**:
- Distributed pool with 2 nodes (node1, node2)
- Pool has been used (some items created/fetched)
- replicas = 5 (not default 10)

**When**:
```lisp
(add-node-to-distributed-pool
  *dist-pool*
  '(:name "node3"
    :capacity 10
    :threshold 2
    :item-maker #'make-connection
    :item-destroyer #'close-connection))
```

**Then**:
- Node count increases from 2 to 3
- `node3` appears in `(distributed-pool-nodes *dist-pool*)`
- Hash ring grows by 5 entries (node3 × 5 replicas)
- Hash ring is re-sorted (deterministic order)
- `node3` pool is created with specified config (capacity=10, threshold=2)
- Some keys that previously mapped to node1/node2 now map to node3
- No error is signaled

**Verification**:
```lisp
(assert (= 3 (length (distributed-pool-nodes *dist-pool*))))
(assert (find "node3" (distributed-pool-nodes *dist-pool*)
              :key #'pool-name :test #'string=))
```

**Source**: tests/test-pooler.lisp:61-98, add-node-to-distributed-pool contract
**Confidence**: 0.9

---

## Scenario 2: Key Redistribution After Adding Node

**Given**:
- Distributed pool with 2 nodes
- Key "user:alice" mapped to node1 before addition

**When**:
- Add node3 to distributed pool
- Fetch with key "user:alice" again

**Then**:
- Key MAY remap to different node (consistent hashing redistributes ~33% of keys)
- OR key stays on node1 (67% of keys don't move)
- Whichever node it maps to after addition, it will consistently map there
- Repeated fetches with "user:alice" go to same node (deterministic post-addition)

**Key Insight**: Topology changes cause partial key redistribution.

**Source**: ADR-0002 (consistent hashing), add-node-to-distributed-pool contract
**Confidence**: 0.85

---

## Scenario 3: Remove Node from Distributed Pool

**Given**:
- Distributed pool with 3 nodes (node1, node2, node3)
- Node2 has items in its pool (current-size > 0)

**When**:
```lisp
(remove-node-from-distributed-pool *dist-pool* "node2")
```

**Then**:
- Node count decreases from 3 to 2
- `node2` NOT in `(distributed-pool-nodes *dist-pool*)`
- Hash ring size reduced by 10 entries (node2 × 10 replicas removed)
- Keys that mapped to node2 now reroute to node1 or node3
- **Items in node2's pool are orphaned** (not destroyed, not migrated)
- No error is signaled (removal always succeeds)

**Orphaned Items**:
- node2's pool object still exists (not GC'd yet)
- Items in node2's queue are inaccessible via distributed pool API
- Items will be GC'd when node2's pool is GC'd (no references)

**Source**: tests/test-pooler.lisp:61-98, remove-node-from-distributed-pool contract, ADR-0004
**Confidence**: 0.9

---

## Scenario 4: Remove Node That Doesn't Exist

**Given**:
- Distributed pool with nodes (node1, node2)

**When**:
```lisp
(handler-case
  (remove-node-from-distributed-pool *dist-pool* "node999")
  (pooler::pool-error (e)
    :caught-error))
```

**Then**:
- `pool-error` is signaled
- Error message indicates node not found
- Pool state unchanged (node1, node2 still present)

**Source**: tests/test-pooler.lisp:89-98, remove-node-from-distributed-pool contract
**Confidence**: 0.95

---

## Scenario 5: Add Node with Duplicate Name

**Given**:
- Distributed pool with node named "node1"

**When**:
```lisp
(handler-case
  (add-node-to-distributed-pool
    *dist-pool*
    '(:name "node1" :item-maker #'make-item))
  (pooler::pool-error (e)
    :caught-error))
```

**Then**:
- `pool-error` is signaled
- Error message indicates duplicate node name
- Pool state unchanged (original node1 still present, no duplicate)

**Source**: add-node-to-distributed-pool contract (error conditions)
**Confidence**: 0.85

---

## Scenario 6: Add Node with Missing Required Config

**Given**:
- Distributed pool exists

**When**:
```lisp
(handler-case
  ;; Missing :item-maker (required)
  (add-node-to-distributed-pool
    *dist-pool*
    '(:name "node-incomplete"))
  (pooler::pool-error (e)
    :caught-error))
```

**Then**:
- `pool-error` is signaled
- Error message indicates missing required key (`:item-maker`)
- No node is added to pool

**Source**: tests/test-pooler.lisp:142-150, add-node-to-distributed-pool contract
**Confidence**: 0.9

---

## Scenario 7: Manual Drain Before Node Removal

**Given**:
- Distributed pool with node "node2"
- node2 has 5 items (current-size = 5)
- User wants to remove node2 without orphaning items

**When**:
```lisp
(let ((node (find "node2" (distributed-pool-nodes *dist-pool*)
                  :key #'pool-name :test #'string=)))
  ;; Wait for natural drain (items returned by users)
  (loop while (> (pool-current-size node) 0)
        do (sleep 1))
  ;; Now remove
  (remove-node-from-distributed-pool *dist-pool* "node2"))
```

**Then**:
- Loop waits until node2's pool is empty
- When `current-size` = 0, removal proceeds
- No items orphaned (all were returned)
- node2 removed cleanly

**Consideration**: Loop may wait indefinitely if items never returned.

**Source**: remove-node-from-distributed-pool contract (usage examples), ADR-0004
**Confidence**: 0.85

---

## Scenario 8: Force Pool-Init Before Removal

**Given**:
- Distributed pool with node "node2"
- node2 has items (current-size > 0)
- User wants immediate removal, doesn't care about items

**When**:
```lisp
(let ((node (find "node2" (distributed-pool-nodes *dist-pool*)
                  :key #'pool-name :test #'string=)))
  ;; Destroy all items in node
  (pool-init node)
  ;; Now remove (node is empty)
  (remove-node-from-distributed-pool *dist-pool* "node2"))
```

**Then**:
- `pool-init` destroys all items in node2 (calls `item-destroyer` on each)
- node2's `current-size` = threshold (regrows after destruction)
- Actually, need to call pool-init again or set threshold to 0
- node2 removed from distributed pool
- No orphaned items (all destroyed)

**Better Approach**:
```lisp
;; Set threshold to 0 to prevent regrowth
(setf (pool-threshold node) 0)
(pool-init node)  ; Destroys all, regrows 0
(remove-node-from-distributed-pool *dist-pool* "node2")
```

**Source**: remove-node-from-distributed-pool contract (usage examples), ADR-0004
**Confidence**: 0.8

---

## Scenario 9: Scale Up During High Load

**Given**:
- Distributed pool with 3 nodes, all heavily used
- Application detects high load

**When**:
```lisp
(dotimes (i 3)
  (add-node-to-distributed-pool
    *dist-pool*
    `(:name ,(format nil "scale-node-~A" i)
      :capacity 20
      :threshold 5
      :item-maker ,#'make-worker
      :item-destroyer ,#'stop-worker)))
```

**Then**:
- 3 new nodes added (total now 6 nodes)
- Hash ring size increases (6 nodes × 10 replicas = 60 entries)
- Keys redistribute: ~50% of keys reroute to new nodes
- New capacity available immediately
- Brief lock held during each addition (serialized)

**Performance Impact**:
- Each addition rebuilds hash ring (sort operation)
- Distributed pool locked during rebuild (blocks concurrent fetches)
- Consider batching or adding during low-traffic period

**Source**: Inferred from add-node-to-distributed-pool contract + ADR-0002
**Confidence**: 0.75

---

## Scenario 10: Scale Down During Low Load

**Given**:
- Distributed pool with 6 nodes
- Application detects low load, wants to reduce capacity

**When**:
```lisp
;; Remove 3 nodes
(dolist (node-name '("scale-node-0" "scale-node-1" "scale-node-2"))
  (remove-node-from-distributed-pool *dist-pool* node-name))
```

**Then**:
- 3 nodes removed (total now 3 nodes)
- Hash ring size decreases
- Keys on removed nodes reroute to remaining nodes
- Items on removed nodes are orphaned (unless manually drained first)
- Next fetch for rerouted keys goes to new node (auto-grows as needed)

**Recommendation**: Drain nodes before removal during low-traffic period.

**Source**: Inferred from remove-node-from-distributed-pool contract + ADR-0004
**Confidence**: 0.75

---

## Summary

These scenarios cover:
- ✓ Adding nodes dynamically
- ✓ Removing nodes dynamically
- ✓ Key redistribution on topology changes
- ✓ Error conditions (duplicate names, missing config, node not found)
- ✓ Orphaned item handling (manual drain, force destroy)
- ✓ Scale up/down patterns

**Key Learnings**:
1. Node addition is safe but causes key redistribution (~1/N keys reroute)
2. Node removal orphans items unless manually drained first
3. Topology changes briefly lock distributed pool (serialized operations)
4. Users must choose drain strategy (wait, force-destroy, or accept orphaning)

**Related**:
- ADR-0002: Consistent hashing minimizes redistribution
- ADR-0004: Node removal limitation (no automatic migration)
