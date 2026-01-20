---
type: ontology
status: draft
confidence: 0.9
last_updated: 2026-01-20
extracted_from: code structure analysis
---

# Pooler Ontology

Relationships and hierarchies between concepts in the pooler library.

## Entity Hierarchy

```
Pool Management
├── Pool (basic)
│   ├── Configuration
│   │   ├── Capacity
│   │   ├── Threshold
│   │   └── Timeout
│   ├── Lifecycle Hooks
│   │   ├── Item Maker
│   │   └── Item Destroyer
│   ├── State
│   │   ├── Queue (contains Pool Items)
│   │   ├── Lock
│   │   └── Last Access
│   └── Metrics
│       ├── Current Size
│       ├── Total Uses
│       ├── Total Created
│       └── Total Pool Inits
│
└── Distributed Pool
    ├── Nodes (list of Pools)
    ├── Hash Ring (list of Replicas)
    ├── Replicas (virtual nodes)
    └── Lock

Pool Item
└── (user-defined type)
```

## Composition Relationships

### Pool CONTAINS

- **Queue**: FIFO storage of Pool Items
  - Relationship: aggregation
  - Cardinality: 1 pool → 1 queue → 0..capacity items
  - Lifecycle: queue lifetime = pool lifetime

- **Lock**: Thread synchronization primitive
  - Relationship: composition
  - Cardinality: 1 pool → 1 lock
  - Purpose: protect pool state mutations

- **Metrics**: Usage and lifecycle counters
  - Relationship: part-of
  - Cardinality: 1 pool → 1 set of metrics
  - Lifecycle: persist across operations, reset on pool-init (except total-pool-inits)

### Distributed Pool CONTAINS

- **Nodes**: List of independent Pool instances
  - Relationship: aggregation
  - Cardinality: 1 distributed-pool → 1..N nodes
  - Lifecycle: nodes can be added/removed dynamically

- **Hash Ring**: Consistent hashing structure
  - Relationship: composition
  - Cardinality: 1 distributed-pool → 1 hash-ring → (N nodes × replicas) virtual nodes
  - Lifecycle: updated when nodes added/removed

## Dependency Relationships

### Pool DEPENDS ON

- **Item Maker** (function)
  - Type: required dependency
  - When: pool creation, item growth
  - Failure: signals pool-item-creation-error

- **Item Destroyer** (function)
  - Type: required dependency
  - When: item disposal, pool-init
  - Failure: silently caught (ignore-errors)

- **Platform Abstractions** (queue, lock)
  - Type: required dependency
  - When: all operations
  - Provided by: utils.lisp

### Distributed Pool DEPENDS ON

- **Pool** (as node implementation)
  - Type: composition dependency
  - Each node IS A regular pool
  - Inherits all pool dependencies

- **Hash Ring** (for routing)
  - Type: algorithmic dependency
  - Used to: map keys to nodes
  - Algorithm: consistent hashing

## Operational Relationships

### Pool Operations

```
fetch-from
├── checks → Timeout (if exceeded, triggers pool-init)
├── checks → Queue (if empty, triggers grow-pool)
├── updates → Current Size (decrement)
├── updates → Total Uses (increment)
└── updates → Last Access (current time)

return-to
├── checks → Capacity (if at limit, destroys item instead of enqueuing)
├── updates → Current Size (increment if enqueued)
└── updates → Last Access (current time, only if enqueued)

grow-pool
├── invokes → Item Maker (N times)
├── updates → Queue (enqueue new items)
├── updates → Current Size (increment)
├── updates → Total Created (increment)
└── updates → Last Access (current time)

pool-init
├── invokes → Item Destroyer (for all items)
├── resets → Queue (new empty queue)
├── resets → Current Size (0)
├── resets → Total Uses (0)
├── resets → Total Created (0)
├── increments → Total Pool Inits
├── signals → Pool Warning
└── invokes → grow-pool (to threshold)
```

### Distributed Pool Operations

```
fetch-from-distributed-pool
├── uses → Key (to determine node)
├── invokes → Hash Ring lookup
├── identifies → Node
└── delegates → fetch-from (on selected node)

return-to-distributed-pool
├── uses → Key (to determine node)
├── invokes → Hash Ring lookup
├── identifies → Node
└── delegates → return-to (on selected node)

add-node-to-distributed-pool
├── creates → Pool (new node)
├── adds → Node (to nodes list)
└── updates → Hash Ring (add virtual nodes)

remove-node-from-distributed-pool
├── removes → Node (from nodes list)
├── updates → Hash Ring (remove virtual nodes)
└── WARNING → orphans items in removed node (known limitation)
```

## State Transitions

### Pool States

```
Empty Pool
    │
    ├─ fetch-from ──▶ grows by Threshold ──▶ Partial Pool
    │
    └─ grow-pool ───▶ Partial Pool

Partial Pool (0 < size < capacity)
    │
    ├─ fetch-from ──▶ Partial Pool or Empty Pool
    ├─ return-to ───▶ Partial Pool or Full Pool
    └─ timeout exceeded ──▶ pool-init ──▶ Partial Pool (threshold)

Full Pool (size = capacity)
    │
    ├─ fetch-from ──▶ Partial Pool
    └─ return-to ───▶ destroys item, stays Full Pool

Any State
    │
    └─ pool-init ───▶ Partial Pool (threshold)
```

### Distributed Pool Topology

```
Distributed Pool
    │
    ├─ add-node ────▶ expands nodes list, rebuilds hash ring
    │                 (keys redistribute partially)
    │
    └─ remove-node ─▶ shrinks nodes list, rebuilds hash ring
                      (keys redistribute, items in removed node orphaned)
```

## Consistency Rules

### Pool Invariants

1. **Size Bounds**: `0 ≤ current-size ≤ capacity` (always)
2. **Queue Consistency**: `queue.length == current-size` (always)
3. **Metric Monotonicity**: `total-uses`, `total-created` increase until pool-init
4. **Timeout Relationship**: `timeout-expired = (current-time - last-access > timeout)`

### Distributed Pool Invariants

1. **Hash Ring Sorted**: Hash ring always sorted by hash value (ascending)
2. **Ring Size**: `hash-ring.length == nodes.length × replicas`
3. **Node Uniqueness**: Each node has unique name
4. **Deterministic Mapping**: Same key always maps to same node (if topology unchanged)

## Error Condition Relationships

```
pool-error (base)
├── pool-item-creation-error
│   └── raised when: Item Maker function errors
│
└── pool-empty-error (defined but unused)

pool-warning (base)
└── raised when: Pool Init occurs
```

## Temporal Relationships

### Lifecycle Events

```
Pool Creation
    ↓
[Optional] Initial Growth
    ↓
Active Use (fetch/return cycles)
    │
    ├─ Timeout Triggers ──▶ Pool Init ──▶ Regrowth
    │                         ↓
    │                    [continues]
    ↓
Pool Destruction (GC)
```

### Key Timing Points

- **Last Access**: Updated on most operations, drives timeout
- **Timeout Check**: Occurs lazily on fetch (not continuous monitoring)
- **Pool Init**: Synchronous (blocks until complete)
- **Item Creation**: Synchronous (happens during fetch if needed)

## Cross-Feature Relationships

### Platform Abstraction ↔ Core Pooling

- Platform Abstraction provides: Queue implementation, Lock implementation
- Core Pooling uses: Platform-abstracted primitives
- Relationship: layered architecture (pooling built on platform layer)

### Core Pooling ↔ Distributed Pooling

- Distributed Pooling uses: Pool as node implementation
- Relationship: distributed pool is a coordinator of basic pools
- Key difference: Distributed pool adds key-based routing, basic pool does not

### Error Handling ↔ All Features

- All features can raise: pool-error, pool-item-creation-error
- Distributed pool additionally raises: pool-error for empty ring, missing nodes
- Pool Init signals: pool-warning

### Thread Safety ↔ All Features

- All features use: Locks for state protection
- Basic pool uses: pool.lock
- Distributed pool uses: distributed-pool.lock (for topology), node.lock (for items)

## Design Decision Relationships

The following decisions in `canon/core/decisions/` explain key relationships:

- **0001-pool-timeout-lazy-checking.md**: Why timeout checking happens on fetch (not continuously)
- **0002-consistent-hashing.md**: Why hash ring is used (vs simple modulo)
- **0003-api-consolidation.md**: Why fetch-from exists but fetch-from-aux doesn't (historical evolution)
- **0004-node-removal-limitation.md**: Why remove-node orphans items (implementation trade-off)

---

## Notes

This ontology captures the structural and behavioral relationships extracted from the codebase. Some relationships are explicit (in code structure), others are implicit (in behavior patterns).

**Confidence**: High (0.9) - based on code structure analysis and test verification

**See Also**:
- canon/core/foundation/vocabulary.md - Term definitions
- canon/features/core-pooling/contracts/ - Pool operation contracts
- canon/features/distributed-pooling/contracts/ - Distributed pool operation contracts
