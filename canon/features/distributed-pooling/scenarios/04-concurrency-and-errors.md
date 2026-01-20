---
type: scenario
feature: distributed-pooling
category: concurrency_and_errors
status: draft
confidence: 0.8
---

# Concurrency and Error Handling Scenarios

Scenarios covering thread safety, concurrent operations, and error conditions in distributed pools.

## Scenario 1: Concurrent Fetches from Different Threads

**Given**:
- Distributed pool with 3 nodes
- Multiple threads fetching with different keys

**When**:
```lisp
#+sbcl
(let ((threads nil))
  (dotimes (i 10)
    (push (sb-thread:make-thread
            (lambda ()
              (let ((key (format nil "user-~A" (random 1000))))
                (let ((item (fetch-from-distributed-pool *dist-pool* key)))
                  (sleep 0.01) ; Simulate work
                  (return-to-distributed-pool *dist-pool* key item))))
            :name (format nil "worker-~A" i))
          threads))
  (mapc #'sb-thread:join-thread threads))
```

**Then**:
- All fetches complete successfully
- No race conditions (each thread gets valid item)
- Items are returned to correct nodes (key consistency)
- Pool state is consistent after all threads finish
- No deadlocks occur

**Thread Safety**: Distributed pool lock protects hash ring access, individual node locks protect pool operations.

**Source**: Inferred from distributed pool threading model + basic pool thread safety tests
**Confidence**: 0.8

---

## Scenario 2: Concurrent Node Addition and Fetches

**Given**:
- Distributed pool with nodes
- Thread A adding new node
- Thread B concurrently fetching items

**When**:
```lisp
#+sbcl
(let ((add-thread (sb-thread:make-thread
                     (lambda ()
                       (add-node-to-distributed-pool
                         *dist-pool*
                         '(:name "new-node"
                           :item-maker #'make-item)))))
      (fetch-thread (sb-thread:make-thread
                      (lambda ()
                        (dotimes (i 100)
                          (fetch-from-distributed-pool
                            *dist-pool* (format nil "key-~A" i)))))))
  (sb-thread:join-thread add-thread)
  (sb-thread:join-thread fetch-thread))
```

**Then**:
- Node addition completes successfully
- All fetches complete successfully
- Distributed pool lock serializes operations:
  - Either fetch waits for add to complete, or add waits for fetches
- No corruption of hash ring
- Some fetches may go to new node (if added before their fetch)

**Performance Note**: Node addition holds distributed pool lock, blocks concurrent fetches briefly.

**Source**: Inferred from lock-based concurrency model, add-node-to-distributed-pool contract
**Confidence**: 0.75

---

## Scenario 3: Error - Create Distributed Pool Without item-maker

**Given**:
- Attempting to create distributed pool without item-maker

**When**:
```lisp
(handler-case
  (make-distributed-pool
    :name "BrokenPool"
    :node-configs '((:name "node1")))
  (pooler::pool-error (e)
    (format t "Error: ~A~%" e)
    :caught))
```

**Then**:
- `pool-error` is signaled
- Error message indicates missing `item-maker`
- No distributed pool is created
- No nodes are created

**Required**: Either global `:item-maker` or `:item-maker` in each node-config.

**Source**: tests/test-pooler.lisp:125-133
**Confidence**: 0.9

---

## Scenario 4: Error - Fetch from Empty Hash Ring

**Given**:
- Distributed pool with all nodes removed

**When**:
```lisp
(defvar *pool* (make-distributed-pool
                 :node-configs '((:name "n1"))
                 :item-maker #'make-item))

;; Remove all nodes
(remove-node-from-distributed-pool *pool* "n1")

;; Try to fetch
(handler-case
  (fetch-from-distributed-pool *pool* "key1")
  (pooler::pool-error (e)
    :caught))
```

**Then**:
- `pool-error` is signaled
- Error message: "Hash ring is empty" or "No nodes available"
- No item is returned

**Source**: tests/test-pooler.lisp:134-138, fetch-from-distributed-pool contract
**Confidence**: 0.95

---

## Scenario 5: Error - Add Node with Missing item-maker in Config

**Given**:
- Distributed pool created without global `item-maker`
- Attempting to add node without `item-maker` in config

**When**:
```lisp
;; Create pool with node-specific item-makers (no global)
(defvar *pool*
  (make-distributed-pool
    :node-configs '((:name "n1" :item-maker #'make-item))))

;; Try to add node without item-maker
(handler-case
  (add-node-to-distributed-pool
    *pool*
    '(:name "n2")) ; Missing :item-maker
  (pooler::pool-error (e)
    :caught))
```

**Then**:
- `pool-error` is signaled
- Error message: "Missing required key :item-maker"
- Node "n2" is NOT added to pool
- Pool state unchanged

**Source**: tests/test-pooler.lisp:142-150, add-node-to-distributed-pool contract
**Confidence**: 0.9

---

## Scenario 6: Handling item-maker Failure During Fetch

**Given**:
- Distributed pool with node whose `item-maker` sometimes fails
- `tries` parameter set to 3

**When**:
```lisp
(let ((call-count 0))
  (defvar *flaky-pool*
    (make-distributed-pool
      :node-configs '((:name "node1"))
      :item-maker (lambda ()
                    (incf call-count)
                    (if (< call-count 3)
                        (error "Simulated failure")
                        :success))))

  (fetch-from-distributed-pool *flaky-pool* "key1" :tries 3))
```

**Then**:
- First attempt fails (item-maker errors)
- Second attempt fails (item-maker errors)
- Third attempt succeeds (item-maker returns :success)
- Item :success is returned
- Retry logic delegated to underlying pool's `fetch-from`

**Source**: fetch-from-distributed-pool contract + fetch-from retry behavior
**Confidence**: 0.8

---

## Scenario 7: Handling item-maker Failure Beyond Retry Limit

**Given**:
- Distributed pool with node whose `item-maker` always fails
- `tries` parameter set to 2

**When**:
```lisp
(defvar *broken-pool*
  (make-distributed-pool
    :node-configs '((:name "node1"))
    :item-maker (lambda () (error "Always fails"))))

(handler-case
  (fetch-from-distributed-pool *broken-pool* "key1" :tries 2)
  (pooler::pool-item-creation-error (e)
    :caught-creation-error)
  (error (e)
    :caught-generic-error))
```

**Then**:
- Both attempts fail (item-maker errors)
- `pool-item-creation-error` or generic `error` is signaled
- No item is returned
- Pool state unchanged

**Source**: fetch-from contract (error conditions), fetch-from-distributed-pool contract
**Confidence**: 0.85

---

## Scenario 8: Concurrent Node Additions (Serialization)

**Given**:
- Distributed pool
- Two threads simultaneously adding different nodes

**When**:
```lisp
#+sbcl
(let ((thread1 (sb-thread:make-thread
                 (lambda ()
                   (add-node-to-distributed-pool
                     *dist-pool*
                     '(:name "node-A" :item-maker #'make-item)))))
      (thread2 (sb-thread:make-thread
                 (lambda ()
                   (add-node-to-distributed-pool
                     *dist-pool*
                     '(:name "node-B" :item-maker #'make-item))))))
  (sb-thread:join-thread thread1)
  (sb-thread:join-thread thread2))
```

**Then**:
- Both nodes added successfully
- Operations are serialized (lock held during each addition)
- Hash ring rebuilt twice (once per addition)
- No race condition on hash ring structure
- Final state: both node-A and node-B present

**Performance**: Sequential additions due to lock (not parallelized).

**Source**: add-node-to-distributed-pool contract (thread safety notes), inferred
**Confidence**: 0.75

---

## Scenario 9: Return After Node Removed (Edge Case)

**Given**:
- Distributed pool with node "node1"
- Item fetched from node1 with key "user:alice"
- User holds item reference
- node1 is removed from distributed pool

**When**:
```lisp
;; Fetch item
(defvar *item* (fetch-from-distributed-pool *dist-pool* "user:alice"))
;; user:alice maps to node1

;; Remove node1
(remove-node-from-distributed-pool *dist-pool* "node1")

;; Try to return item
(return-to-distributed-pool *dist-pool* "user:alice" *item*)
```

**Then**:
- Key "user:alice" now maps to different node (node2 or node3)
- Item is returned to NEW node (not original node1)
- **Pool state anomaly**: Item from node1 stored in node2's pool
- No error signaled (return succeeds)
- Future fetches from "user:alice" may return wrong item

**Risk**: Low (requires holding item reference across topology change).

**Mitigation**: Return items before topology changes, or use short-lived fetches.

**Source**: Inferred from return-to-distributed-pool contract + remove-node behavior
**Confidence**: 0.7

---

## Scenario 10: Pool Timeout Triggers pool-init on Underlying Node

**Given**:
- Distributed pool with node "node1"
- node1 has timeout = 10 seconds
- Pool idle for 15 seconds

**When**:
```lisp
;; Create distributed pool with short timeout
(defvar *pool*
  (make-distributed-pool
    :node-configs '((:name "node1"))
    :default-timeout 10
    :item-maker #'make-item))

;; Fetch, establish last-access
(fetch-from-distributed-pool *pool* "key1")
;; Wait for timeout
(sleep 15)
;; Fetch again
(fetch-from-distributed-pool *pool* "key1")
```

**Then**:
- First fetch succeeds normally
- After 15 seconds, node1's pool is "old" (timeout exceeded)
- Second fetch triggers `pool-init` on node1
- `pool-warning` is signaled (from pool-init)
- node1's pool is reinitialized (all items destroyed, regrown to threshold)
- Fetch succeeds with newly created item

**Property**: Timeout behavior inherited from underlying pool implementation.

**Source**: fetch-from contract (timeout checking), pool-init contract, ADR-0001
**Confidence**: 0.85

---

## Summary

These scenarios cover:
- ✓ Concurrent fetches (thread safety)
- ✓ Concurrent topology changes (serialization via locks)
- ✓ Error conditions (missing config, empty ring, creation failures)
- ✓ Retry behavior on failures
- ✓ Edge cases (return after node removal, timeout behavior)

**Key Error Conditions**:
1. `pool-error`: Missing item-maker, duplicate node name, empty hash ring, node not found
2. `pool-item-creation-error`: item-maker function fails beyond retry limit
3. `pool-warning`: Timeout triggers pool-init on underlying node

**Thread Safety Model**:
- Distributed pool lock: Protects hash ring access and node list
- Individual node locks: Protect each pool's queue and counters
- Operations are thread-safe but serialized (one topology change at a time)
- Fetches can proceed concurrently on different nodes

**Related**:
- Basic pool concurrency tests (in test/pooler-test.lisp)
- ADR-0001: Timeout behavior
- Contract error condition sections
