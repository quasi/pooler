# Concepts: Understanding Pooling

A guide to the core ideas behind resource pooling.

## The Pooling Problem

### The Expensive Resource

Some resources are slow to create:

| Resource | Creation Time | Use Time | Ratio |
|----------|---------------|----------|-------|
| Database Connection | 100ms | 5ms | 20:1 |
| TLS Connection | 50ms | 10ms | 5:1 |
| Thread | 10ms | 1ms | 10:1 |
| File Handle | 1ms | 0.1ms | 10:1 |

When creation time dominates, your application spends more time connecting than actually working.

### The Naive Approach (No Pooling)

```
User Request
    ↓
Create DB Connection (100ms)
    ↓
Execute Query (5ms)
    ↓
Close Connection (10ms)
    ↓
User Response (115ms total)
```

If you have 100 concurrent users, you're spending 10 seconds per request cycle just on connection management.

## The Pooling Solution

### Pre-Create and Reuse

```
Startup:
  Create Connection Pool (with 5 pre-created connections)
      ↓
  Ready to handle requests

User Request #1
    ↓
  Get Connection from Pool (instant)
    ↓
  Execute Query (5ms)
    ↓
  Return Connection to Pool (instant)
    ↓
  Response (5ms total)

User Request #2
    ↓
  Get Connection from Pool (instant - another connection ready)
    ↓
  Execute Query (5ms)
    ↓
  Return Connection (instant)
    ↓
  Response (5ms total)
```

**Result**: Hundreds of requests with minimal overhead (no connection creation per request).

## Key Pooling Concepts

### 1. Capacity

The **maximum** number of resources the pool can hold.

```
Pool with capacity 10:
[X] [X] [X] [X] [X] [ ] [ ] [ ] [ ] [ ]
 1   2   3   4   5   6   7   8   9   10
```

**Why limit capacity?**
- Prevent runaway resource allocation
- Resources have costs (memory, file descriptors, connections)
- Database has max connection limit anyway

**Too high**: Wastes resources, pools grow unnecessarily
**Too low**: Requests queue up waiting for connections

### 2. Threshold

The **minimum** number of idle resources to maintain.

```
Pool with threshold 2:

Normal state:
[X] [X] [ ] [ ] [ ] [ ] [ ] [ ] [ ] [ ]
 Ready to use immediately
```

**Why maintain minimum?**
- Avoid creation delays on request spikes
- 2-5 pre-created connections eliminate "cold start"

**Calculation**: Usually 1/5 of capacity:
- Capacity 20 → threshold 4
- Capacity 10 → threshold 2
- Capacity 5 → threshold 1

### 3. Capacity vs. Threshold

These work together:

```
Scenario 1: Pool below threshold
  ├─ Available: 1 connection (below threshold of 3)
  ├─ Request comes in
  ├─ Pool creates 3 more connections (back to threshold)
  └─ Request gets connection immediately

Scenario 2: Pool at threshold
  ├─ Available: 3 connections (at threshold)
  ├─ Request comes in
  ├─ Pool has spare connection
  └─ No new creation needed, instant response

Scenario 3: Pool above threshold but below capacity
  ├─ Available: 5 connections (above threshold of 2, below capacity of 10)
  ├─ Request comes in
  ├─ Pool has spare
  └─ Instant response, no creation

Scenario 4: Pool at capacity
  ├─ Available: 0 connections (10/10 in use, at capacity)
  ├─ Request comes in
  ├─ Request must wait for someone to return a connection
  └─ No new connections can be created (at limit)
```

### 4. Return and Destroy

What happens when you return a connection?

```
Case 1: Pool has room (below capacity)
  ├─ Returned connection enqueued
  ├─ Available for next request
  └─ Pool size increases

Case 2: Pool is full (at capacity)
  ├─ Returned connection is immediately destroyed
  ├─ Never stored in queue
  └─ Pool size unchanged
  └─ Why? Prevent resource exhaustion
```

### 5. Idle Timeout

Resources become stale. Connections timeout after DB inactivity.

```
Timeline:
T=0:     Pool created, 3 connections made
T=5min:  Last request processed, pool goes idle
T=10min: Someone requests a connection
         ├─ Pool checks: idle time = 10 min > timeout (5 min)?
         ├─ YES! All connections might be stale
         ├─ Pool refreshes: destroy all, recreate threshold
         └─ Request gets fresh connection

Why?     Without timeout, you'd get stale connections
         - Database closed connection after 5 mins
         - Pool still thinks it's valid
         - Queries fail mysteriously
```

## Distributed Pooling

When a single pool isn't enough.

### The Scaling Problem

```
Single Database:
  ┌─────────────┐
  │ DB Server   │
  │ (Max: 100   │
  │  connections)
  └─────────────┘
       ↑
  Pool of 20
       ↑
   Web Servers ← Limited by DB connection limit
```

As you add web servers, each with a connection pool, you hit the database's limit.

### The Distributed Solution

```
Shard 1: 100 connections ─┐
Shard 2: 100 connections ─┼─ Can handle 3x the traffic!
Shard 3: 100 connections ─┘

Consistent Hashing Routes:
  User 123     → Shard 1
  User 456     → Shard 2
  User 789     → Shard 3
  User 999     → Shard 1 (always same as user 123)
```

**Key property**: Same key always goes to same shard.

### How Consistent Hashing Works

Imagine a circular hash ring:

```
                 0
              ↗     ↖
          360°         90°
         /                \
      (Node A)        (Node B)
        |                  |
      270°              180°
         \                /
          ↘     ↙
          Shard C
```

**Key lookup**:
1. Hash the key: `hash("user:123")` = 45°
2. Find next node clockwise: Node B at 90°
3. Route to Node B

**If a node fails**:
- Some keys redistribute (to next node in ring)
- Most keys stick with their existing node
- Much better than naive `user_id % num_servers`

## Thread Safety

Pools must be thread-safe for concurrent requests.

```
Thread 1: fetch() ──┐
Thread 2: return()  ├─ All must coordinate!
Thread 3: grow()    ├─ Can't have race conditions
Thread 4: fetch()  ──┘
```

**Pooler's approach**: Lock during operations.

```
fetch():
  Lock pool
  Check queue
  Dequeue if available
  Unlock
  [Create item if needed - outside lock]
  Return item

return():
  Lock pool
  Check capacity
  Enqueue or destroy
  Unlock
```

**Trade-off**: Brief locks prevent data corruption but reduce concurrent throughput slightly.

## Mental Models

### Model 1: Restaurant Waiters

```
Pool = Restaurant
Capacity = Total waiters on staff (10)
Threshold = Waiters on duty at opening (2)
Items = Individual waiters
Timeout = Time before waiters on break go home
```

- More customers coming? Waiters return from break (grow pool)
- All waiters busy? Customers wait in line (queue)
- Restaurant closes? All waiters go home (pool-init)

### Model 2: Library Checkout System

```
Pool = Library
Capacity = Total books available (100)
Threshold = Books on display (5)
Items = Individual books
Timeout = Books go to deep storage if unused (refresh)
```

- Borrower wants a book? Check the display first (fetch)
- Return a book? Goes back on display if space (return)
- Books on display for years? Archive them (pool-init)

### Model 3: Hotel Check-In

```
Pool = Hotel
Capacity = Total rooms (50)
Threshold = Rooms cleaned and ready (10)
Items = Individual rooms
Timeout = Rooms checked/refresh after period of vacancy
```

- Guest arriving? Give them a ready room (fetch)
- Guest leaving? Room goes back to cleaning queue (return/destroy)
- All rooms occupied? Guest waits or goes elsewhere (capacity limit)

## Common Questions

### "Why not just make everything unlimited?"

**Response**: Resources have costs.
- Connections consume memory
- Open files use system resources
- Threads use CPU time
- Database has hard limits

Unlimited growth crashes the system.

### "Why not make threshold equal to capacity?"

**Response**: Pre-create everything? That's wasteful.
- Memory used whether needed or not
- Application startup time increases
- Most applications don't need all resources immediately

Threshold balances startup cost vs. availability.

### "What if my requests are very slow?"

**Response**: Increase capacity (more concurrent requests can run).
```
Scenario: Requests take 30 seconds, threshold 2, capacity 5
  Requests/sec = 5 / 30 = 0.166 requests/sec max

If 1000 requests/sec arrive, they queue up.
Solution: Increase capacity: 100, get ~3.3 requests/sec

But: More connections = more resources!
```

### "Can I mix thread pools with connection pools?"

**Response**: Yes, they're different things.
```
Thread Pool (Executor)       Connection Pool
─────────────────────────    ─────────────────
Manages worker threads       Manages connections
Who does the work           What they work with
```

You might have:
- 20 worker threads (thread pool)
- 30 database connections (connection pool)
- 20 workers share 30 connections

## See Also

- [Core Pooling Tutorial](../tutorials/core-pooling.md) - Hands-on examples
- [Distributed Pooling Tutorial](../tutorials/distributed-pooling.md) - Scaling guide
- [Architecture Guide](../architecture.md) - Technical details
