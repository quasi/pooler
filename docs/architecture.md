# Architecture Guide

This guide explains Pooler's design, key decisions, and mental models.

## Core Concepts

### What is Pooling?

Pooling is a pattern for managing expensive-to-create resources:

```
Without Pooling              With Pooling
─────────────────────────────────────────────
Create → Use → Destroy       Create → Keep → Reuse → Reuse → ...
  100ms    5ms   100ms            100ms      5ms     5ms     5ms
────────────────────────        Total time much less
  ~210ms per operation
```

Database connections exemplify this: creating a connection might take 100ms, but querying takes only 5ms. Reusing connections saves massive overhead.

**Pooler's approach**:
1. Pre-create a set of connections (the "pool")
2. When you need one, fetch it (instant)
3. Use it
4. Return it (instant)
5. Repeat

### Pool Lifecycle

```
┌─────────────┐
│   Created   │  Pool starts with 0 items
└──────┬──────┘
       │
       ├──→ Fetch → Grows to threshold
       │
       ├──→ Idle > timeout → Pool-init → Fresh items
       │
       ├──→ At capacity + return → Item destroyed
       │
       └──→ Use with with-pool → Automatic cleanup
```

## Design Decisions

### Decision 1: Lazy Timeout Checking

**Question**: How do we detect and refresh stale connections?

**Options**:
A) Check every connection continuously (background thread)
B) Check on demand (only on fetch) ✓ Chosen

**Why Option B**:
- No background threads needed
- Simpler code, fewer race conditions
- Sufficient for most use cases (timeout is typically minutes)
- Lower CPU overhead

**Trade-off**: Stale connections aren't detected until someone tries to fetch.

**Implementation**:
```lisp
(defun fetch-from (pool)
  (when (pool-is-old? pool)        ; Check if idle too long
    (pool-init pool))              ; Refresh all items
  ;; ... proceed with fetch
  )
```

### Decision 2: Consistent Hashing for Distribution

**Question**: How do we distribute requests across multiple nodes?

**Options**:
A) Round-robin (simple, loses affinity)
B) Modulo-based sharding (efficient, brittle on topology changes)
C) Consistent hashing (complex, resilient) ✓ Chosen

**Why Option C**:
- Same key always routes to same node (essential for data consistency)
- When topology changes, only ~1/N keys move (not all)
- Proven technique (used by Memcached, DynamoDB, etc.)

**How it works**:
```
Hash Ring (sorted by position):
  ├─ Position 100 → Node A
  ├─ Position 250 → Node B
  ├─ Position 400 → Node C
  └─ Position 900 → Node A

Key lookup: hash(key) % ring_size → find next position clockwise
  hash("user:123") = 350 → Node C
  hash("user:456") = 50  → Node A (wraps around)
```

**Replicas**: Each node gets N virtual positions (replicas) on the ring for better distribution.

### Decision 3: Thread-Safe by Default

Pooler uses locks to ensure thread-safety:

**Per-pool lock**: Guards all pool operations (fetch, return, grow, init)

**Distributed pool lock**: Guards hash ring modifications (add/remove nodes)

**Trade-off**: Slight performance cost for safety guarantees.

### Decision 4: No Automatic Item Validation

**Question**: Should we test items before returning them?

**Options**:
A) Test every returned item (slower, reliable)
B) Trust items are valid (faster, assumes correctness)
C) Offer optional validation (flexible)

**Why Option B** (with timeout fallback):
- Item-maker should guarantee item validity
- Validation adds overhead to every fetch
- Timeout mechanism handles stale items
- Applications needing validation can test manually

## Internal Architecture

### Pool Structure

```lisp
(defstruct pool
  name              ; "My Pool"
  queue             ; FIFO storage (platform-specific)
  lock              ; Thread synchronization
  item-maker        ; fn to create items
  item-destroyer    ; fn to cleanup items
  capacity          ; max items
  threshold         ; min items target
  timeout           ; idle seconds before refresh
  last-access       ; timestamp
  current-size      ; items in queue now
  total-created     ; items ever made
  total-uses        ; fetches
  total-pool-inits) ; refreshes
```

### Distributed Pool Structure

```lisp
(defstruct distributed-pool
  name              ; "My Shards"
  nodes             ; List of pool objects
  hash-ring         ; Sorted vector of ring nodes
  replicas          ; Virtual nodes per physical node
  lock)             ; Guard for ring modifications
```

### Hash Ring Entry

```lisp
(defstruct hash-ring-node
  id                ; Node name (e.g., "shard-1")
  hash-value)       ; Position on ring
```

## Platform Abstraction

Pooler abstracts platform-specific concurrency primitives:

```
────────────────────────────────────────────
  Pooler API (queue operations, locks)
────────────────────────────────────────────
       ↓
     ┌─────────────────────────────┐
     │ Platform Abstraction Layer  │
     └─────────────────────────────┘
       ↓
SBCL        Allegro       Others
(native)    (native)      (fallback)
```

**Queue abstraction** (`utils.lisp`):
- SBCL: `sb-concurrency:queue`
- Allegro: `mp:queue`
- Fallback: Pure Lisp list + synchronization

**Lock abstraction**:
- SBCL: `sb-thread:mutex`
- Allegro/Others: `bordeaux-threads:lock`

## Memory Management

### What Gets GC'd?

**Pooled items**: Held in queue until:
- Returned to full pool (destroyed immediately)
- Pool re-initializes (destroyed and replaced)
- GC happens (if pool is GC'd)

**Pool object**: Lives as long as you hold a reference to it.

**Distributed pool**: Same as regular pool.

### No Memory Leaks If...

✓ You return items via `return-to` or `with-pool`
✓ You destroy pools by dropping references

❌ If you fetch but never return, item stays in queue forever

## Performance Characteristics

### Fetch Operation

```
Average case: O(1)
  - Dequeue from queue
  - Increment counter

Slow case: O(threshold)
  - Pool empty: grow pool (create threshold items)

Slowest case: O(capacity)
  - Pool old: pool-init (destroy all, recreate threshold)
```

### Distributed Pool Fetch

```
Average case: O(log N) where N = replicas × nodes
  - Hash key
  - Binary search on sorted hash ring
  - Fetch from node pool (O(1))

Total: O(log N) + O(1) = O(log N)
```

## Concurrency Model

### When Locks Are Held

```
Fetch:           Hold lock for brief moment
  ├─ Check queue
  └─ If empty, release, grow, re-acquire

Return:          Hold lock briefly
  ├─ Check capacity
  ├─ Enqueue or destroy

Grow:            Hold lock briefly per item
  └─ Create and enqueue

Distributed:     Lock only when accessing hash ring
  └─ Per-node fetch uses node's lock
```

**Design**: Hold locks as briefly as possible to maximize concurrency.

## Testing Philosophy

Pooler's test suite covers:

1. **Basic Operations**: Create, fetch, return, destroy
2. **Capacity & Threshold**: Growth, limits, overflow
3. **Timeout & Re-init**: Staleness detection, refresh
4. **Thread-Safety**: Concurrent operations from multiple threads
5. **Distributed**: Hashing, node management, consistency
6. **Error Handling**: Creation failures, edge cases

Key invariant tested: **Same key always maps to same node** (distributed pools).

## When to Use Each Feature

### Use Basic Pool When...
- Single resource backend
- No distribution needed
- Simplicity preferred

### Use Distributed Pool When...
- Multiple backend nodes
- Need horizontal scaling
- Data locality matters (user sharding, etc.)
- Consistent routing required

### Never Use Pool For...
- Unbounded resource allocation (set capacity!)
- Synchronous work that can block indefinitely
- Resources with complex lifecycle (use wrapper class)

## Future Directions

Possible enhancements (not implemented):

1. **Pool prioritization**: Some requests get priority access
2. **Dynamic sizing**: Adjust capacity based on load
3. **Multi-master**: Distributed pools with write to multiple nodes
4. **Pool metrics export**: Prometheus/Grafana integration
5. **Item health checks**: Optional validation before returning items
6. **Smart timeout**: Timeout based on item health, not just time

## References

- **Consistent Hashing**: [Original paper](https://www.akamai.com/us/en/multimedia/documents/technical-publication-consistent-hashing-and-random-trees-technical-publication-pdf.pdf)
- **Thread-Safe Patterns**: [The Art of Multiprocessor Programming](https://www.elsevier.com/books/the-art-of-multiprocessor-programming/herlihy/978-0-12-415950-1)
- **Common Lisp Concurrency**: [SBCL Manual - Threads](http://www.sbcl.org/manual/#Threading)
