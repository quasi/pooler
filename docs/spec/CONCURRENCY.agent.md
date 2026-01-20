---
type: specification
version: 1.0.0
applies-to: [concurrency, thread-safety]
requires: [core-pooling]
domain: synchronization
---

# Concurrency and Thread-Safety Specification

Machine-oriented concurrency semantics, lock requirements, and race condition prevention.

## Terminology

- **pool-lock**: Mutex protecting pool state (queue, metrics, last-access)
- **distributed-pool-lock**: Mutex protecting hash ring and nodes list
- **atomic operation**: Operation that appears indivisible to other threads
- **critical section**: Code holding lock
- **race condition**: Concurrent operations causing inconsistent state
- **deadlock**: Two or more threads waiting indefinitely for each other's locks

## Concurrency Model

```
┌──────────────────┐
│  Multiple Threads │
├──────────────────┤
│  Thread 1: fetch │
│  Thread 2: return│  All competing for same pool
│  Thread 3: grow  │
│  Thread 4: fetch │
└──────────────────┘
         ↓
    ┌─────────┐
    │ pool-lock (mutex)
    └─────────┘
         ↓
  ┌─────────────────┐
  │ Queue           │
  │ Metrics         │ Only one thread in critical section at a time
  │ last-access     │
  └─────────────────┘
```

## Invariants

### INV-C001: No Queue Corruption

`Queue state is consistent (no duplicates, proper FIFO order) despite concurrent operations`

**Enforced by**: Pool-lock held during all queue operations
**Check**: Iterate queue post-operation, verify FIFO order
**Consequence of violation**: fetch returns duplicate item, race conditions

### INV-C002: No Metrics Corruption

`All metrics (total-uses, total-created, total-pool-inits) are monotonically increasing, never corrupted by concurrent updates`

**Enforced by**: Increment-under-lock discipline
**Check**: Metrics never decrease between observations
**Consequence of violation**: Misleading utilization statistics

### INV-C003: Atomicity of Capacity Check

`Checking if pool is full and inserting/destroying item MUST be atomic`

**Enforced by**: Single lock held for both operations
**Check**: No gap between check and action
**Consequence of violation**: Item leaked when capacity exceeded, or too many items inserted

### INV-C004: No Task Starvation

`All waiting threads eventually acquire lock (no indefinite starvation)`

**Enforced by**: Fair mutex (FIFO queue)
**Check**: Monitor thread wait times
**Consequence of violation**: Some threads never complete, cascading failures

## Normative Rules

### RULE-C001: Lock During Queue Access

**Rule**: ALL queue operations (enqueue, dequeue, empty-check, length) MUST hold pool-lock.

**Applies to**: `fetch-from`, `return-to`, `grow-pool`, `pool-init`

**Lock scope**: BEGIN just before queue access, END immediately after

**Implementation**:
```lisp
(with-pool-lock ((pool-lock pool))
  ;; Critical section: queue operations only
  (let ((item (dequeue queue)))
    item))  ; Exit lock
;; Long operations outside lock
```

**Rationale**: Prevent concurrent queue modification

**Violation consequence**: Lost items, INV-C001 violated, data corruption

**Agent action**: Auto-fix forbidden, architectural requirement

### RULE-C002: Release Lock Before Item-Maker/Destroyer

**Rule**: Item-maker and item-destroyer calls MUST occur with pool-lock RELEASED.

**Applies to**: `grow-pool`, `fetch-from`, `pool-init`

**Rationale**: Item-maker/destroyer may block (I/O, network) for 100ms+. Holding lock serializes all threads.

**Lock pattern**:
```lisp
(loop for x from 1 to grow-by
  do (with-pool-lock ((pool-lock pool))
       (when (< (pool-current-size pool) (pool-capacity pool))
         (incf (pool-total-created pool))))
     ;; Lock RELEASED here
     ;; Now create item (may take 100ms)
     (let ((item (funcall item-maker)))
       (with-pool-lock ((pool-lock pool))
         ;; Re-acquire lock to enqueue
         (enqueue (pool-queue pool) item)
         (incf (pool-current-size pool)))))
```

**Violation consequence**:
- Holding lock during I/O: All other threads blocked for 100ms
- Serialized fetches despite having multiple items available
- Defeats multi-threaded pool benefits

**Agent action**: Auto-fix forbidden, critical for performance

### RULE-C003: Atomic Fetch or Grow

**Rule**: `fetch-from` MUST either:
  - Fetch existing item (atomic under lock), OR
  - Release lock, grow pool, re-acquire lock, retry

**Applies to**: `fetch-from` operation

**Guarantee**: No fetch hangs indefinitely

**Implementation**:
```lisp
(loop for attempt from 1 to tries
  do (let ((item (with-pool-lock ((pool-lock pool))
                   (dequeue (pool-queue pool)))))
       (if item
         (return item)
         (progn
           (grow-pool pool)  ; Outside lock
           ;; Loop retries
           ))))
```

**Rationale**: Guarantee forward progress, handle spurious wake-ups

**Violation consequence**: Deadlock potential, fetch hangs

**Agent action**: Auto-fix forbidden, must be explicit

### RULE-C004: No Nested Locks (Distributed Pool)

**Rule**: When holding distributed-pool-lock, MUST NOT acquire individual node pool locks.
When holding node pool-lock, MUST NOT try distributed-pool-lock (potential deadlock).

**Applies to**: Mixed distributed pool + node pool operations

**Pattern**: Distribute operations by level
```lisp
;; Level 1: Distributed pool operations (hash ring)
(with-lock ((distributed-pool-lock pool))
  ;; Only ring modifications here, no node access
  )

;; Level 2: Node operations (inside level-1 lock is OK for quick checks)
(dolist (node nodes)
  ;; Each node's operations use that node's lock, not distributed-pool-lock
  (with-pool-lock ((pool-lock node))
    ;; Node operations
    ))
```

**Rationale**: Prevent circular wait deadlocks

**Violation consequence**: Potential deadlock, threads hang indefinitely

**Agent action**: Auto-fix forbidden, architectural review required

### RULE-C005: Lock Timeout Not Specified

**Rule**: Pool locks MUST NOT use timeouts (wait indefinitely for lock).

**Applies to**: `with-pool-lock` macro

**Rationale**: Timeout-based locks complicate error handling, hide deadlocks

**Violation consequence**: Spurious timeouts on high load, confusing failures

**Agent action**: Auto-fix forbidden, forbidden transformation

## Anti-Patterns

### ANTI-C001: Double-Locking

**Description**: Thread attempts to acquire same lock twice

**Symptoms**:
- Thread A: acquire lock
- Thread A: call function that also tries to acquire same lock
- Deadlock: Thread A waiting for itself

**Why harmful**: Immediate deadlock

**Remediation**: Use non-reentrant locks or restructure code to avoid recursive locking

**Agent action**: Red flag, block merge

### ANTI-C002: Lock Held During Blocking I/O

**Description**: Thread holding pool-lock while performing network/disk I/O

**Symptoms**:
- Thread A: fetches, holds lock, calls item-maker (100ms)
- Thread B: tries to return, waits on lock
- Thread C: tries to fetch, waits on lock
- All blocked while Thread A waits for I/O

**Why harmful**: Serializes concurrent operations, defeats pooling benefits

**Remediation**: RULE-C002 - release lock before I/O

**Agent action**: Yellow flag if item-maker takes >10ms, suggest refactoring

### ANTI-C003: Lock Order Inconsistency

**Description**: Different threads acquire locks in different orders

```
Thread A: acquire distributed-lock, then node-lock
Thread B: acquire node-lock, then distributed-lock
         → Circular wait → Deadlock
```

**Why harmful**: Circular wait, potential deadlock

**Remediation**: Define lock order (distributed first, then node) and enforce consistently

**Agent action**: Red flag, requires architectural review

### ANTI-C004: No Lock at All

**Description**: Concurrent access without synchronization

**Symptoms**:
- Multiple threads reading/writing queue without lock
- Metrics incremented without atomic operations
- Queue corrupted, items duplicated or lost

**Why harmful**: Race conditions, data corruption

**Remediation**: RULE-C001 - always hold lock during queue access

**Agent action**: Red flag, block merge

## Heuristics

### HEUR-C001: Lock Contention

**Signal**: Average lock wait time > 10ms
**Confidence**: Medium (70%)
**Interpretation**: High contention, possible bottleneck
**Action**: Yellow flag, profile lock usage

### HEUR-C002: Item Creation Overhead

**Signal**: fetch-from average time > 100ms
**Confidence**: Medium (65%)
**Interpretation**: Possibly holding lock during item creation
**Action**: Yellow flag, check RULE-C002 compliance

### HEUR-C003: Successful Multi-Threaded Access

**Signal**: Multiple threads accessing pool concurrently, no deadlocks
**Confidence**: High (95%)
**Interpretation**: Concurrency model is sound
**Action**: Green signal, continue

## Exemplar Pattern: Thread-Safe Pool Usage

### PATTERN-C001: Multi-Threaded Fetch/Return

**Scenario**: Web application with 10 worker threads sharing one pool.

**Complete Example**:

```lisp
;; Create pool (shared across threads)
(defvar *db-pool*
  (pooler:make-pool
    :capacity 20
    :threshold 5
    :item-maker (lambda () (connect-to-db))
    :item-destroyer (lambda (c) (disconnect c))))

;; Worker threads
(defun worker-thread (thread-id)
  (loop for iteration from 1 to 100
    do (pooler:with-pool (connection *db-pool*)
         ;; Each thread: fetch (waits if empty), use, auto-return
         (let ((result (query connection "SELECT 1")))
           (format t "[T~A ~A] Result: ~A~%" thread-id iteration result)))
    ;; Back to loop, fetch next connection
    ))

;; Start 10 threads
(defvar *threads*
  (loop for i from 1 to 10
    collect (make-thread
              (lambda () (worker-thread i))
              :name (format nil "worker-~A" i))))

;; Threads now competing for connections
;; If all 20 connections in use: 11-20 wait for return
;; As returns complete, waiting threads acquire connections
;; Lock ensures queue consistency, no items lost or duplicated
;; RULE-C001: All queue operations under lock
;; RULE-C002: item-maker/destroyer outside lock (DB I/O doesn't block pool)
```

**Rules Satisfied**: RULE-C001 (lock during queue access), RULE-C002 (I/O outside lock), INV-C001 (no queue corruption)

**Why This Shape**:
- `with-pool` handles fetch/return automatically
- Caller doesn't manage lock
- Lock contention minimized (brief critical section)
- I/O doesn't block other threads

**Variations**:

| Scenario | Modification |
|---|---|
| Dynamic thread count | Add thread pool that adjusts worker count |
| Error handling | Wrap with-pool body in error handler |
| Metrics | Log fetch/return times per thread |

**Anti-pattern**:

```lisp
;; DON'T: Manual lock management
(sb-thread:with-mutex ((pooler::pool-lock *db-pool*))
  ;; Thread holding lock
  (let ((conn (pooler:fetch-from *db-pool*)))
    ;; Lock still held!
    (query conn "SELECT 1")  ; 100ms query
    ;; Other threads blocked for 100ms
    (pooler:return-to *db-pool* conn)))
;; Lock released

;; Violates RULE-C002, serializes concurrent requests
```

### PATTERN-C002: Distributed Pool Topology Changes

**Scenario**: Add node to distributed pool while threads are fetching.

**Complete Example**:

```lisp
;; Active distributed pool
(defvar *shards*
  (pooler:make-distributed-pool
    :name "User Shards"
    :node-configs (list (list :name "s1") (list :name "s2"))))

;; Threads fetching continuously
(defun fetch-worker (worker-id)
  (loop
    do (pooler:with-pool (conn *shards* (format nil "user:~A" (random 1000)))
         (query conn "SELECT 1"))))

;; Meanwhile: Add new shard (no blocking of fetch operations)
(defun add-shard-thread ()
  ;; This acquires distributed-pool-lock briefly
  ;; Fetch threads don't block (they only hold per-node locks)
  (pooler:add-node-to-distributed-pool
    *shards*
    (list :name "s3" :capacity 15)))

;; Key insight:
;; - distributed-pool-lock: Brief hold during ring update
;; - per-node pool-lock: Held only during individual node access
;; - fetch-worker threads: Blocked only during their specific node operation
;; - No global pool freeze, only momentary ring lock
```

**Rules Satisfied**: RULE-C004 (no nested locks), INV-C004 (no starvation)

**Why This Shape**:
- Ring lock (distributed-pool-lock) held briefly
- Node operations independent
- Fetches continue on unmodified nodes
- Minimal disruption during topology changes

## Machine Checklist

```
Lock Discipline:
  [ ] All queue operations (enqueue, dequeue, check-empty) hold pool-lock (RULE-C001)
  [ ] Item-maker calls occur with lock RELEASED (RULE-C002)
  [ ] Item-destroyer calls occur with lock RELEASED (RULE-C002)
  [ ] No nested pool-lock acquisitions (ANTI-C001)
  [ ] No lock timeout (RULE-C005)

Atomic Operations:
  [ ] Fetch-or-grow is atomic (check, grow outside lock, retry) (RULE-C003)
  [ ] Capacity check + insert/destroy is atomic (RULE-C003, INV-C003)
  [ ] Metrics increments are atomic (INV-C002)

Thread-Safety:
  [ ] No race conditions on current-size (INV-C001)
  [ ] No race conditions on total-uses (INV-C002)
  [ ] No race conditions on total-created (INV-C002)
  [ ] No duplicate items in queue (INV-C001)

Distributed Pool:
  [ ] distributed-pool-lock held during ring modifications (RULE-C004)
  [ ] Per-node pool locks independent from distributed-pool-lock (RULE-C004)
  [ ] No circular lock wait (ANTI-C003)
  [ ] No indefinite starvation (INV-C004)
```

## Lock Hierarchy

Ordered from highest to lowest precedence (always acquire in this order to prevent deadlock):

1. **distributed-pool-lock** (if using distributed pool)
2. **per-node pool-lock** (acquired only after distributed-pool-lock if needed)

Reverse order never allowed:
- Thread must not acquire per-node lock, then try distributed-pool-lock

## Performance Characteristics

| Operation | Lock Time | Total Time | Constraint |
|---|---|---|---|
| fetch (hit) | ~1µs (dequeue) | ~10µs | Brief lock, fast |
| fetch (miss, grow) | ~1µs check | ~100ms+ (item creation) | I/O outside lock |
| return (stored) | ~1µs (enqueue) | ~10µs | Brief lock, fast |
| return (destroyed) | ~1µs check | ~10ms+ (destroy) | I/O outside lock |
| pool-init | ~10µs (reset) | ~500ms+ (recreate) | I/O outside lock |

Key: Lock held for microseconds, operations for milliseconds.

---

**Derived from**: canon/features/core-pooling/ thread-safety scenarios
**Version**: 1.0.0
**Status**: Agent-facing spec
