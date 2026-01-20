---
type: vocabulary
status: draft
confidence: 0.92
last_updated: 2026-01-20
extracted_from: triangulation (docs + code + tests)
---

# Pooler Vocabulary

Core terms and concepts for the pooler library.

## Basic Pooling Concepts

### Pool

**Definition**: A container for managing reusable objects with automatic lifecycle management.

**Components**:
- Queue: FIFO storage for available pool items
- Lock: Thread synchronization primitive
- Item Maker: User-supplied function to create new items
- Item Destroyer: User-supplied function to dispose items
- Configuration: capacity, threshold, timeout
- Metrics: current-size, total-uses, total-created, total-pool-inits, last-access

**Source**: README.md:17, pooler.lisp:48-61
**Confidence**: Very High (1.0)

**Struct Definition** (pooler.lisp:48-61):
```lisp
(defstruct pool
  name          ; identifying string
  queue         ; item storage
  lock          ; thread safety
  item-maker    ; creation function
  item-destroyer ; cleanup function
  capacity      ; max items (fixnum, default 40)
  threshold     ; min idle items (fixnum, default 2)
  timeout       ; idle seconds before re-init (fixnum, default 300)
  last-access   ; timestamp (integer)
  current-size  ; items in pool (fixnum)
  total-uses    ; fetch count (fixnum)
  total-created ; creation count (fixnum)
  total-pool-inits) ; re-init count (fixnum)
```

---

### Pool Item

**Definition**: A reusable object with heavy creation cost that is managed by a pool.

**Purpose**: Represent expensive resources like database connections, file handles, or complex objects that benefit from reuse rather than repeated creation/destruction.

**Characteristics**:
- Type is user-defined (determined by item-maker function)
- Created via pool's item-maker function
- Destroyed via pool's item-destroyer function
- Lifecycle managed entirely by the pool

**Examples**:
- Database connection
- Network socket
- Cryptographic context
- Compiled template
- Thread/worker

**Source**: README.md:9, implicit throughout codebase
**Confidence**: Very High (1.0)

---

### Capacity

**Definition**: The maximum number of pool items a pool can hold concurrently.

**Purpose**: Limit resource consumption and prevent unbounded growth.

**Behavior**:
- Pool growth stops when capacity is reached
- Items returned to a full pool are immediately destroyed (not stored)
- Affects concurrency potential (more items = more concurrent operations)

**Default**: 40 items
**Type**: fixnum (non-negative integer)

**Source**: README.md:24, pooler.lisp:54
**Confidence**: Very High (1.0)

---

### Threshold

**Definition**: The minimum number of idle pool items the pool attempts to maintain.

**Purpose**: Ensure availability of pre-created items for immediate use without waiting for creation.

**Behavior**:
- Pool grows by threshold amount when empty
- pool-init regrows to threshold after cleanup
- Acts as the "warm pool" size

**Default**: 2 items
**Type**: fixnum (non-negative integer)

**Source**: README.md:25, pooler.lisp:55
**Confidence**: Very High (1.0)

---

### Timeout

**Definition**: Number of seconds of pool idleness after which the pool automatically re-initializes.

**Purpose**: Handle stale or expired resources (e.g., database connections that timeout).

**Behavior**:
- Checked lazily on fetch operations (not continuously)
- Triggers pool-init when exceeded
- Reset on most pool operations (fetch, return, grow)

**Default**: 300 seconds (5 minutes)
**Type**: fixnum (non-negative integer, seconds)

**Rationale**: See canon/core/decisions/0001-pool-timeout-lazy-checking.md

**Source**: README.md:26, pooler.lisp:56
**Confidence**: Very High (1.0)

---

### Pool Init

**Definition**: The process of destroying all existing pool items and recreating the threshold number of fresh items.

**Purpose**: Refresh the pool with new items when existing items may be stale or invalid.

**Behavior**:
1. Fetch and destroy all existing items
2. Reset metrics (current-size, total-uses, total-created → 0)
3. Increment total-pool-inits counter
4. Signal pool-warning condition
5. Grow pool to threshold size

**Triggers**:
- Timeout exceeded (automatic)
- Manual invocation via pool-init function

**Source**: README.md:66-68, pooler.lisp:306-320
**Confidence**: Very High (1.0)

---

## Distributed Pooling Concepts

### Distributed Pool

**Definition**: A pool that distributes items across multiple independent pool instances (nodes) using consistent hashing to route requests.

**Purpose**: Enable horizontal scaling of pooled resources across multiple backend nodes.

**Components**:
- Nodes: List of regular pool instances
- Hash Ring: Consistent hashing structure for key-to-node mapping
- Replicas: Number of virtual nodes per physical node
- Lock: Thread synchronization for node/ring access

**Key Difference from Pool**: Requires a key parameter for all operations to determine which node to use.

**Source**: pooler.lisp:63-68, git commit cead997
**Confidence**: High (0.85)
**Status**: Code-only (not in README)

**Struct Definition** (pooler.lisp:63-68):
```lisp
(defstruct distributed-pool
  name       ; identifying string
  nodes      ; list of pool instances
  hash-ring  ; sorted list of hash-ring-node objects
  replicas   ; virtual nodes per physical node (fixnum, default 10)
  lock)      ; thread safety
```

---

### Hash Ring

**Definition**: A consistent hashing data structure that maps keys to nodes in a distributed pool.

**Purpose**: Provide consistent key-to-node mapping with minimal disruption when nodes are added or removed.

**Implementation**:
- Sorted list of hash-ring-node objects (by hash value)
- Each physical node has multiple virtual nodes (replicas)
- Key lookup finds first node with hash ≥ key hash (circular wraparound)

**Properties**:
- Always sorted by hash value
- Size = number of nodes × replicas per node
- Deterministic: same key always maps to same node (unless topology changes)

**Source**: pooler.lisp:70-109
**Confidence**: High (0.85)
**Rationale**: See canon/core/decisions/0002-consistent-hashing.md

---

### Node

**Definition**: An individual pool instance within a distributed pool.

**Purpose**: Act as an independent pooling unit in a distributed system.

**Characteristics**:
- Is a standard pool struct (full pool capabilities)
- Identified by name (used in hash ring)
- Has own capacity, threshold, timeout, item-maker, item-destroyer
- Operates independently of other nodes

**Lifecycle**:
- Created during distributed pool initialization
- Can be added dynamically via add-node-to-distributed-pool
- Can be removed via remove-node-from-distributed-pool (orphans items)

**Source**: pooler.lisp:119-136 (creation), CLAUDE.md
**Confidence**: High (0.85)

---

### Replica

**Definition**: A virtual node in the hash ring representing a physical node at a different hash position.

**Purpose**: Improve key distribution uniformity across nodes by giving each physical node multiple positions on the hash ring.

**Implementation**:
- For each physical node, create N virtual nodes (replicas)
- Each replica has a unique hash position (based on "{node-id}-{replica-index}")
- All replicas for a node point to the same underlying pool

**Default**: 10 replicas per physical node
**Type**: fixnum (positive integer)

**Trade-off**: More replicas = better distribution but larger hash ring.

**Source**: pooler.lisp:67, 82-87
**Confidence**: High (0.8)

---

## Lifecycle Terms

### Item Maker

**Definition**: User-supplied function that creates a new pool item.

**Signature**: `() → item`

**Requirements**:
- Takes no arguments
- Returns a new pool item
- Should not error (or pool signals pool-item-creation-error)

**Example**:
```lisp
(lambda () (clsql:connect '("127.0.0.1" "db" "user" "pass")
                          :database-type :mysql))
```

**Source**: pooler.lisp:52, README.md:22
**Confidence**: Very High (1.0)

---

### Item Destroyer

**Definition**: User-supplied function that cleanly disposes of a pool item.

**Signature**: `(item) → ignored`

**Requirements**:
- Takes one argument (the item to destroy)
- Performs cleanup (close connections, free resources, etc.)
- Return value ignored
- Errors are silently caught (ignore-errors wrapper)

**Example**:
```lisp
(lambda (connection) (clsql:disconnect :database connection))
```

**Source**: pooler.lisp:53, README.md:23
**Confidence**: Very High (1.0)

---

## Metrics Terms

### Current Size

**Definition**: The current number of items in the pool's queue (available for fetch).

**Range**: 0 to capacity
**Type**: fixnum

**Changes when**:
- Increments: item returned to pool (if below capacity)
- Decrements: item fetched from pool
- Resets to 0: pool-init

**Source**: pooler.lisp:58
**Confidence**: Very High (1.0)

---

### Total Uses

**Definition**: The total number of times items have been fetched from the pool since creation or last pool-init.

**Purpose**: Track pool utilization.

**Behavior**:
- Increments on every successful fetch
- Resets to 0 on pool-init

**Source**: pooler.lisp:59
**Confidence**: Very High (1.0)

---

### Total Created

**Definition**: The total number of pool items created since pool creation or last pool-init.

**Purpose**: Track item creation overhead.

**Behavior**:
- Increments whenever item-maker is invoked
- Resets to 0 on pool-init

**Source**: pooler.lisp:60
**Confidence**: Very High (1.0)

---

### Total Pool Inits

**Definition**: The number of times the pool has been re-initialized.

**Purpose**: Track pool churn and potential stability issues.

**Behavior**:
- Increments on every pool-init
- Never resets (persists across pool-inits)

**Source**: pooler.lisp:61
**Confidence**: Very High (1.0)

---

### Last Access

**Definition**: Timestamp of the last operation that touched the pool.

**Purpose**: Calculate idle time for timeout-based re-initialization.

**Behavior**:
- Updated on: fetch (via fetch-from-aux), return (when enqueued), grow
- NOT updated on: return when item destroyed (at capacity)
- Used to detect timeout condition

**Type**: integer (universal-time)
**Source**: pooler.lisp:57
**Confidence**: Very High (1.0)

---

## Platform Terms

### Platform Abstraction

**Definition**: The layer that provides consistent queue and locking interfaces across different Common Lisp implementations.

**Supported Platforms**:
- SBCL: Uses sb-concurrency:queue and sb-thread:mutex
- Allegro CL: Uses mp:queue and bordeaux-threads
- Fallback: Pure Lisp queue implementation and bordeaux-threads

**Abstraction Points**:
- make-queue, enqueue, dequeue, queue-empty-p (queue operations)
- make-pool-lock, with-pool-lock (locking operations)

**Source**: utils.lisp, CLAUDE.md:68-74
**Confidence**: High (0.9)

---

## Notes

- **High Confidence Terms** (≥0.9): Basic pool vocabulary extensively documented and tested
- **Medium Confidence Terms** (0.7-0.9): Distributed pool terms well-implemented but undocumented
- **Extraction Source**: Multi-source triangulation (README + code + tests + git history)

**See Also**:
- canon/core/foundation/ontology.md - Relationships between terms
- canon/features/core-pooling/ - Basic pool specification
- canon/features/distributed-pooling/ - Distributed pool specification
