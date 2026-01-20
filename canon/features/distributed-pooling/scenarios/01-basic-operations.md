---
type: scenario
feature: distributed-pooling
category: basic_operations
status: draft
confidence: 0.9
---

# Basic Distributed Pool Operations

Scenarios covering fundamental distributed pool usage: creation, fetch, and return.

## Scenario 1: Create Distributed Pool with Multiple Nodes

**Given**:
- 3 node configurations with names "node1", "node2", "node3"
- A global `item-maker` function that creates test items
- A global `item-destroyer` function for cleanup
- Default capacity = 5, threshold = 1 per node

**When**:
```lisp
(defvar *dist-pool*
  (make-distributed-pool
    :name "TestDistPool"
    :node-configs '((:name "node1")
                    (:name "node2")
                    (:name "node3"))
    :default-capacity 5
    :default-threshold 1
    :item-maker (lambda () (make-test-item))
    :item-destroyer (lambda (item) (destroy-test-item item))))
```

**Then**:
- Distributed pool is created
- `(distributed-pool-name *dist-pool*)` = "TestDistPool"
- `(length (distributed-pool-nodes *dist-pool*))` = 3
- Hash ring contains 30 entries (3 nodes × 10 replicas)
- Each node pool has capacity = 5, threshold = 1
- Each node pool's `current-size` = 0 (no items pre-created)

**Source**: tests/test-pooler.lisp:21-58
**Confidence**: 0.95

---

## Scenario 2: Fetch from Distributed Pool Using Key

**Given**:
- Distributed pool with 3 nodes (from Scenario 1)
- Pool is initially empty (all nodes have size = 0)

**When**:
```lisp
(defvar *item-a* (fetch-from-distributed-pool *dist-pool* "user:alice"))
```

**Then**:
- Item is returned (not null)
- Key "user:alice" maps to a specific node (deterministic)
- That node's pool auto-grows by threshold (1 item)
- That node's `current-size` = 0 (1 created - 1 fetched)
- That node's `total-created` = 1
- That node's `total-uses` = 1
- Other nodes remain untouched (`current-size` = 0)

**Source**: tests/test-pooler.lisp:38-52
**Confidence**: 0.9

---

## Scenario 3: Return to Distributed Pool with Same Key

**Given**:
- Item fetched from distributed pool with key "user:alice" (Scenario 2)
- Item currently held by user code

**When**:
```lisp
(return-to-distributed-pool *dist-pool* "user:alice" *item-a*)
```

**Then**:
- Item is returned to the same node that served "user:alice"
- That node's `current-size` = 1 (item back in pool)
- No error is signaled
- Function returns new size of that node's pool

**Source**: tests/test-pooler.lisp:38-52
**Confidence**: 0.9

---

## Scenario 4: Multiple Keys Map to Different Nodes

**Given**:
- Distributed pool with 3 nodes
- Multiple distinct keys

**When**:
```lisp
(defvar *item-a* (fetch-from-distributed-pool *dist-pool* "user:alice"))
(defvar *item-b* (fetch-from-distributed-pool *dist-pool* "user:bob"))
(defvar *item-c* (fetch-from-distributed-pool *dist-pool* "session:xyz"))
```

**Then**:
- All items are returned successfully
- Keys distribute across nodes (not all to same node)
- Each key consistently maps to same node on repeated calls
- `(get-node-for-key *dist-pool* "user:alice")` returns same node every time
- Total `total-created` across all nodes >= 3

**Verification**:
```lisp
(let ((node-a1 (get-node-for-key *dist-pool* "user:alice"))
      (node-a2 (get-node-for-key *dist-pool* "user:alice")))
  (assert (eq node-a1 node-a2))) ; Same node for same key
```

**Source**: tests/test-pooler.lisp:38-52, inferred from contracts
**Confidence**: 0.85

---

## Scenario 5: Fetch with Retry on Transient Failure

**Given**:
- Distributed pool with nodes
- `item-maker` that fails on first call, succeeds on second call

**When**:
```lisp
(let ((call-count 0))
  (defvar *pool-with-retries*
    (make-distributed-pool
      :node-configs '((:name "node1"))
      :item-maker (lambda ()
                    (incf call-count)
                    (if (< call-count 2)
                        (error "Transient failure")
                        :item))
      :item-destroyer #'identity))

  (fetch-from-distributed-pool *pool-with-retries* "key1" :tries 3))
```

**Then**:
- Item is successfully returned
- `item-maker` was called 2 times (failed once, succeeded once)
- Retry logic delegated to underlying `fetch-from` in node pool

**Source**: Inferred from fetch-from-distributed-pool contract + fetch-from retry behavior
**Confidence**: 0.8

---

## Scenario 6: Fetch from Empty Distributed Pool (No Nodes)

**Given**:
- Distributed pool with no nodes (all nodes removed or created with empty node-configs)

**When**:
```lisp
(handler-case
  (fetch-from-distributed-pool *empty-dist-pool* "key1")
  (pooler::pool-error (e)
    :caught-error))
```

**Then**:
- `pool-error` is signaled
- Error message indicates hash ring is empty or no nodes available

**Source**: tests/test-pooler.lisp:134-138, fetch-from-distributed-pool contract
**Confidence**: 0.9

---

## Scenario 7: Return to Wrong Key (User Error)

**Given**:
- Item fetched with key "user:alice"
- User code attempts to return with different key

**When**:
```lisp
(defvar *item* (fetch-from-distributed-pool *dist-pool* "user:alice"))
;; User mistakenly uses different key for return
(return-to-distributed-pool *dist-pool* "user:bob" *item*)
```

**Then**:
- Item is returned to node for "user:bob" (not "user:alice")
- **Pool state corrupted**: Item from nodeA stored in nodeB
- No immediate error (silent corruption)
- Future fetches may return wrong items

**Anti-Pattern**: This demonstrates incorrect usage.

**Correct Pattern**:
```lisp
(let ((key "user:alice"))
  (let ((item (fetch-from-distributed-pool *dist-pool* key)))
    (unwind-protect
      (use item)
      (return-to-distributed-pool *dist-pool* key item))))
```

**Source**: return-to-distributed-pool contract (warnings section)
**Confidence**: 1.0

---

## Scenario 8: with-pool Equivalent for Distributed Pool

**Given**:
- Distributed pool with nodes
- Key for routing

**When**:
```lisp
;; User creates helper macro
(defmacro with-distributed-pool-item ((var pool key) &body body)
  `(let ((,var (fetch-from-distributed-pool ,pool ,key)))
     (unwind-protect
       (progn ,@body)
       (return-to-distributed-pool ,pool ,key ,var))))

;; Usage
(with-distributed-pool-item (conn *db-pool* "user:alice")
  (query-database conn "SELECT * FROM users"))
```

**Then**:
- Item is fetched for key "user:alice"
- Body executes with item
- Item is returned to same key even if body signals error
- Cleanup is guaranteed (unwind-protect)

**Source**: Inferred from with-pool contract + distributed pool contracts
**Confidence**: 0.85

---

## Summary

These scenarios cover:
- ✓ Basic pool creation with multiple nodes
- ✓ Key-based fetch and return operations
- ✓ Consistent key-to-node mapping
- ✓ Retry behavior
- ✓ Error conditions (empty pool)
- ✓ Common mistakes (wrong key on return)
- ✓ Safe usage patterns

**Coverage**: Core distributed pool API (`make-distributed-pool`, `fetch-from-distributed-pool`, `return-to-distributed-pool`)

**Missing Coverage** (see other scenario files):
- Node management (add/remove)
- Key redistribution
- Concurrent operations
- Advanced patterns
