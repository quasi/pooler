---
type: specification
version: 1.0.0
applies-to: [core-pooling]
requires: [platform-abstraction]
domain: resource-lifecycle
---

# Core Pooling Specification

Machine-oriented constraint specification for pool creation, lifecycle, and operation.

## Terminology

- **pool**: Container managing reusable items via FIFO queue
- **item**: User-managed object with creation cost
- **capacity**: Maximum items pool can hold (≥ threshold)
- **threshold**: Minimum idle items to maintain (≥ 1)
- **timeout**: Idle seconds before pool-init (≥ 0)
- **current-size**: Count of items in queue (0 ≤ current-size ≤ capacity)
- **item-maker**: Function `() → item` creating new items
- **item-destroyer**: Function `(item) → ignored` destroying items
- **fetch**: Operation retrieving item from pool
- **return**: Operation returning item to pool
- **pool-init**: Operation destroying all items and re-creating threshold count

## Invariants

### INV-001: Capacity Bound

`∀ pool: 0 ≤ current-size(pool) ≤ capacity(pool)`

**Mechanically verifiable**: Yes
**Check**: `(and (>= current-size 0) (<= current-size capacity))`
**Enforced by**: enqueue/dequeue operations, capacity limits
**Consequence of violation**: Thread-safety breakdown, resource exhaustion

### INV-002: Threshold Validity

`∀ pool: 1 ≤ threshold(pool) ≤ capacity(pool)`

**Enforced at**: Pool construction only
**Check**: `(and (>= threshold 1) (<= threshold capacity))`
**Agent action**: Validate at make-pool, reject invalid configs

### INV-003: Non-Negative Metrics

`∀ pool: total-uses ≥ 0 ∧ total-created ≥ 0 ∧ total-pool-inits ≥ 0`

**Enforced by**: Increment-only counters
**Check**: `(every #'(lambda (m) (>= m 0)) (list total-uses total-created total-pool-inits))`
**Consequence of violation**: Misleading metrics, silent corruption

### INV-004: Item Consistency

`∀ item fetched: item was created by this pool's item-maker`

**Enforced by**: Fetch operation only returns enqueued items
**Check**: No direct verification (trust queue integrity)
**Consequence of violation**: Item type mismatch, destroyer failure

### INV-005: Destroyer Executes on Return

`When item returned to full pool (current-size = capacity), item-destroyer is called exactly once`

**Enforced by**: return-to operation
**Check**: Monitor destroyer invocations
**Consequence of violation**: Resource leak (connections never closed)

## Normative Rules

### RULE-001: Pool Must Grow When Empty

**Rule**: `fetch-from` on empty pool MUST create `threshold` items via `item-maker` before returning.

**Applies to**: `fetch-from` operation

**Precondition**: `current-size = 0 ∧ timeout not exceeded`

**Postcondition**: `item-maker` called `threshold` times, one item returned, `current-size = threshold - 1`

**Rationale**: Avoid fetch delays, maintain threshold invariant

**Violation consequence**: Caller gets nil, requests queue indefinitely

**Agent action**: Flag as critical bug, block merge

### RULE-002: Timeout Triggers Pool-Init

**Rule**: On `fetch-from`, if `current-time - last-access > timeout`, call `pool-init` before fetching.

**Applies to**: `fetch-from` operation

**Rationale**: Refresh stale items (e.g., connections closed by database)

**Violation consequence**: Stale connections returned, queries fail mysteriously

**Agent action**: Auto-fix if missing, critical for correctness

### RULE-003: Returned Items Stored or Destroyed

**Rule**: `return-to` MUST either:
  - Enqueue item if `current-size < capacity`, OR
  - Call `item-destroyer` immediately if `current-size = capacity`

**Applies to**: `return-to` operation

**Rationale**: Prevent unbounded growth, guarantee resource cleanup

**Violation consequence**: Resource exhaustion, item leaks

**Agent action**: Auto-fix forbidden, critical path

### RULE-004: Item Maker Errors Signal Pool-Item-Creation-Error

**Rule**: If `item-maker` throws any error, `fetch-from` MUST catch it and signal `pool-item-creation-error`.

**Applies to**: `fetch-from` and `grow-pool` operations

**Retries**: `fetch-from` retries `tries` times on `pool-item-creation-error`

**Rationale**: Explicit error type for client error handling

**Violation consequence**: Unexpected exceptions leak to caller

**Agent action**: Auto-fix forbidden, must be explicit

### RULE-005: Pool-Init Resets Metrics

**Rule**: `pool-init` MUST reset `current-size`, `total-uses`, `total-created` to 0.

**Applies to**: `pool-init` operation

**Note**: `total-pool-inits` counter itself is NOT reset (cumulative)

**Rationale**: Fresh metrics after pool refresh

**Violation consequence**: Misleading utilization statistics

**Agent action**: Auto-fix allowed if missing

### RULE-006: Lock During Queue Operations

**Rule**: All queue operations (enqueue, dequeue, empty check) MUST hold pool-lock.

**Applies to**: `fetch-from`, `return-to`, `grow-pool`, `pool-init`

**Lock scope**: MUST be as brief as possible (release before item-maker/destroyer calls)

**Rationale**: Thread-safety guarantee

**Violation consequence**: Race conditions, corrupted queue state

**Agent action**: Auto-fix forbidden, requires architectural review

### RULE-007: Item-Maker/Destroyer Outside Lock

**Rule**: Calls to `item-maker` and `item-destroyer` MUST occur with pool-lock RELEASED.

**Applies to**: `grow-pool`, `fetch-from`, `pool-init`

**Rationale**: Prevent deadlocks, allow long-running I/O

**Violation consequence**: Deadlocks, thread starvation

**Agent action**: Auto-fix forbidden, critical path

### RULE-008: No Null Returns from Fetch

**Rule**: `fetch-from` MUST return an item object or signal error (never nil/null).

**Applies to**: `fetch-from` public API

**Rationale**: Caller can use result directly without null-checking

**Violation consequence**: NullPointerException in caller

**Agent action**: Auto-fix forbidden, API contract

## Anti-Patterns

### ANTI-001: Unbounded Growth

**Description**: Pool operations that permit current-size > capacity

**Symptoms**:
- Current-size increases beyond capacity
- return-to sometimes stores when capacity is reached
- Capacity limit not checked before enqueue

**Why harmful**: Resource exhaustion, OOM crashes

**Remediation**: Always check capacity, destroy items if full

**Agent action**: Flag as critical bug, block merge

### ANTI-002: Destroyed Item Reuse

**Description**: Returning same item twice to pool

**Symptoms**:
- Item successfully destroyed on first return
- Item returned again, still in queue
- Item-destroyer called with already-freed resource

**Why harmful**: Use-after-free, double-free errors

**Remediation**: Track item identity, reject duplicates in strict mode

**Agent action**: Yellow flag, add test for deduplication

### ANTI-003: Item-Maker Blocks Forever

**Description**: item-maker function never returns or hangs indefinitely

**Symptoms**:
- fetch-from hangs, caller blocked indefinitely
- No timeout mechanism in item-maker
- External resource unavailable (database down)

**Why harmful**: Thread starvation, cascading failures

**Remediation**: Implement timeouts in item-maker, use circuit breaker

**Agent action**: Warn in docs, not auto-detectable

### ANTI-004: Pool-Lock Held During I/O

**Description**: Keeping pool-lock held while calling item-maker/destroyer

**Symptoms**:
- Other threads blocked during connection creation (100ms)
- Serialized item creation even with multi-threaded pool
- Reduced concurrency

**Why harmful**: Defeats concurrent access benefits

**Remediation**: Release lock before I/O, RULE-007

**Agent action**: Auto-fix forbidden, architectural issue

## Allowed Transformations

| Transformation | Scope | Conditions |
|---|---|---|
| Add retry logic to fetch-from | Implementation | Preserves item semantics, respects tries parameter |
| Cache last-access timestamp | Implementation | Update consistently across all operations |
| Optimize queue implementation | Internal | Preserve FIFO ordering, thread-safety |
| Add metrics collection | Observable | No behavior change, purely additive |
| Reduce lock contention | Implementation | Must preserve INV-001 through INV-005 |

## Forbidden Transformations

| Transformation | Reason |
|---|---|
| Return nil from fetch-from on empty | Breaks RULE-008, caller code fails |
| Skip item-destroyer on full pool | Violates INV-005, resource leak |
| Allow current-size > capacity | Violates INV-001, resource exhaustion |
| Remove timeout check from fetch | Violates RULE-002, stale items |
| Hold lock during item-maker call | Violates RULE-007, deadlock risk |

## Heuristics

### HEUR-001: Item Creation Overhead

**Signal**: (total-created / total-uses) > 0.5
**Confidence**: High (80%)
**Interpretation**: >50% of fetches create new items (pool undersized)
**Action**: Green signal to increase capacity

### HEUR-002: Pool Thrashing

**Signal**: total-pool-inits > total-uses / 1000
**Confidence**: Medium (70%)
**Interpretation**: Pool re-initializes frequently (timeout too short)
**Action**: Yellow flag, suggest increasing timeout

### HEUR-003: Stale Connections

**Signal**: (current-time - last-access) >= timeout AND fetch occurs
**Confidence**: High (90%)
**Interpretation**: Pool about to refresh (expected behavior)
**Action**: Green signal, expected pool-init

## Exemplar Pattern: Basic Pool Usage

### PATTERN-001: Safe Fetch-Return Cycle

**Scenario**: Caller needs resource for operation, must guarantee return.

**Complete Example**:

```lisp
;; PATTERN: with-pool macro ensures cleanup
(pooler:with-pool (connection db-pool)
  ;; connection auto-fetched here
  (let ((result (execute-query connection "SELECT 1")))
    ;; operation completes
    result))
;; connection auto-returned here, even on error

;; Equivalent manual pattern (less safe):
(let ((connection (pooler:fetch-from db-pool)))
  (unwind-protect
    (execute-query connection "SELECT 1")
    (pooler:return-to db-pool connection)))
;; Must use unwind-protect to guarantee cleanup
```

**Rules Satisfied**: RULE-001 (fetch), RULE-003 (return), RULE-008 (non-null)

**Why This Shape**:
- `with-pool` guarantees cleanup on normal exit AND exceptions
- Manual pattern requires `unwind-protect` discipline
- Caller never has to track return responsibility

**Variations**:

| Scenario | Modification |
|---|---|
| Multiple operations | Keep connection in let binding, reuse for multiple queries |
| Error handling | Wrap body in handler-case for database-specific errors |
| Metrics | Access connection metadata via pool-name, log after use |

**Anti-pattern**:

```lisp
;; DON'T: No cleanup on error
(let ((conn (pooler:fetch-from pool)))
  (some-operation conn))
;; If some-operation throws, connection NEVER returned
;; Violates RULE-003, ANTI-002 (item lost)
```

### PATTERN-002: Pool Growth and Threshold

**Scenario**: Application startup needs warm pool, minimal latency on first requests.

**Complete Example**:

```lisp
(defun app-startup ()
  ;; Create pool with explicit capacity and threshold
  (setf *db-pool*
    (pooler:make-pool
      :name "Production DB"
      :capacity 20
      :threshold 5  ; Pre-create 5 connections
      :item-maker (lambda ()
                    ;; Connection creation (100ms typical)
                    (pq:connect :host "db.local" :dbname "app"))
      :item-destroyer (lambda (conn)
                        ;; Cleanup called on pool-init or when full
                        (pq:disconnect conn))))
  ;; Threshold automatically grown during make-pool
  ;; 5 connections now ready in pool (took ~500ms)
  (format t "Pool initialized with ~A items ready~%"
          (pooler::pool-current-size *db-pool*)))

;; First request hits pool immediately (item already created)
(pooler:with-pool (conn *db-pool*)
  (pq:query conn "SELECT 1"))
;; Response time: ~5ms (no creation overhead)

;; If capacity exceeded, further returns destroy items
(loop repeat 25
  do (pooler:with-pool (conn *db-pool*)
       (pq:query conn "SELECT 1")))
;; Pool holds max 20, excess connections destroyed on return
```

**Rules Satisfied**: RULE-001 (grow on startup), INV-002 (threshold ≤ capacity)

**Why This Shape**:
- Threshold pre-warmth trades startup time for response time
- Typical: threshold = capacity / 4 to capacity / 5
- Capacity protection prevents resource exhaustion

**Variations**:

| Config | Use Case |
|---|---|
| threshold=1, capacity=5 | Minimal memory, acceptable cold-start latency |
| threshold=10, capacity=20 | High-throughput service, pre-warm investment |
| threshold=capacity | Reserved for embedded/single-user (wasteful on server) |

## Machine Checklist

```
Pool Construction:
  [ ] capacity >= 1
  [ ] threshold >= 1
  [ ] threshold <= capacity
  [ ] item-maker is callable, returns non-nil
  [ ] item-destroyer is callable

Pool Operations:
  [ ] current-size never exceeds capacity (INV-001)
  [ ] total-uses, total-created always >= 0 (INV-003)
  [ ] fetch-from never returns nil (RULE-008)
  [ ] return-to stores item or calls destroyer (RULE-003)
  [ ] pool-init resets current-size, total-uses, total-created (RULE-005)
  [ ] fetch-from respects timeout (RULE-002)
  [ ] Queue operations synchronized with lock (RULE-006)

Thread-Safety:
  [ ] No lock held during item-maker call (RULE-007)
  [ ] No lock held during item-destroyer call (RULE-007)
  [ ] All queue reads/writes protected by lock (RULE-006)

Error Handling:
  [ ] item-maker errors caught as pool-item-creation-error (RULE-004)
  [ ] fetch-from retries up to tries parameter (RULE-001 variants)
  [ ] All errors propagate explicitly (never silenced)
```

## Ambiguity Resolution

Precedence (highest first):
1. Invariants (INV-001 through INV-005)
2. MUST rules (RULE-001 through RULE-008)
3. Anti-patterns (ANTI-001 through ANTI-004)
4. Heuristics
5. Patterns

**Example**: If timeout=0 conflicts with HEUR-002, INV-002 takes precedence (timeout ≥ 0).

## Design Notes

**Lazy timeout checking** (RULE-002): Timeout checked only on fetch, not continuously. This avoids background threads and simplifies concurrency.

**No item validation**: Pool does not test items before returning. Trust item-maker guarantees validity and timeout handles staleness.

**Thread-safety via lock**: Single pool-lock protects all state. More sophisticated schemes (per-bucket locks, lock-free) acceptable if they preserve all invariants.

---

**Derived from**: canon/features/core-pooling/ contracts and scenarios
**Version**: 1.0.0
**Status**: Agent-facing spec
