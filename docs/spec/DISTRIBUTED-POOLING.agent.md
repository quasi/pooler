---
type: specification
version: 1.0.0
applies-to: [distributed-pooling]
requires: [core-pooling]
domain: consistent-hashing
---

# Distributed Pooling Specification

Machine-oriented constraint specification for distributed pools using consistent hashing.

## Terminology

- **distributed-pool**: Pool distributing items across multiple nodes via consistent hashing
- **node**: Individual pool instance managing a subset of items
- **key**: String identifier determining node routing (e.g., "user:123")
- **hash-ring**: Sorted circular structure mapping keys to nodes
- **replica**: Virtual node position on hash ring
- **replicas**: Count of virtual nodes per physical node (default 10)
- **consistent hashing**: Algorithm ensuring same key always routes to same node

## Invariants

### INV-D001: Ring Consistency

`∀ key: hash(key) mapped to node via get-node-for-key always returns same node (unless topology changes)`

**Mechanically verifiable**: Yes
**Check**: `(eq (get-node-for-key pool k1) (get-node-for-key pool k1))`
**Enforced by**: Sorted hash ring, immutable ring between fetches
**Consequence of violation**: Same user key routes to different nodes, data inconsistency

### INV-D002: Ring Sorted

`hash-ring is sorted by hash-value in ascending order`

**Enforced by**: add-node-to-ring, remove-node-from-ring sort operations
**Check**: `(every #'< (mapcar #'hash-value ring) (rest (mapcar #'hash-value ring)))`
**Consequence of violation**: Key lookup returns wrong node (binary search fails)

### INV-D003: Node Presence

`∀ node in nodes: node appears in hash-ring exactly `replicas` times`

**Enforced by**: add-node-to-ring (creates replicas), remove-node-from-ring (removes all)
**Check**: `(= (count node-id ring :key #'hash-ring-node-id) replicas)`
**Consequence of violation**: Uneven distribution, some keys have no valid node

### INV-D004: All Nodes In Nodes List

`∀ ring-entry: corresponding node exists in distributed-pool.nodes`

**Enforced by**: add-node-to-ring validation, remove-node-from-ring cleanup
**Check**: `(every #'(lambda (ring-entry) (find (ring-entry-id ring-entry) nodes :key #'pool-name)) ring)`
**Consequence of violation**: Routing to non-existent node crashes caller

### INV-D005: No Empty Ring

`(length hash-ring) > 0`

**Enforced by**: Require at least 1 node on make-distributed-pool
**Check**: `(> (length hash-ring) 0)`
**Consequence of violation**: No valid node for any key, all operations fail

## Normative Rules

### RULE-D001: Consistent Key Routing

**Rule**: Same key MUST always route to same node until pool topology changes.

**Applies to**: `fetch-from-distributed-pool`, `return-to-distributed-pool`, `get-node-for-key`

**Implementation**: Hash key to position on ring, find next node clockwise

**Rationale**: Data consistency, prevent split-brain writes

**Violation consequence**: User data scattered across shards, queries fail, inconsistency

**Agent action**: Auto-fix forbidden, verify with multiple consecutive fetches

### RULE-D002: Replicas Create Distribution

**Rule**: Each physical node MUST have exactly `replicas` virtual positions on ring.

**Applies to**: add-node-to-ring, hash ring construction

**Rationale**: Even distribution across nodes, minimize hotspots

**Violation consequence**: Some nodes overloaded, others idle

**Agent action**: Auto-fix allowed if replicas wrong

### RULE-D003: Node Addition Updates Ring

**Rule**: `add-node-to-distributed-pool` MUST:
  1. Create new pool instance from node-config
  2. Add `replicas` positions to hash-ring (sorted)
  3. Update distributed-pool.nodes list
  4. Maintain INV-D002, INV-D003, INV-D004

**Applies to**: add-node-to-distributed-pool

**Atomicity**: Operation MUST hold lock for all 4 steps (or fail entirely)

**Rationale**: Prevent partial updates causing orphaned ring entries

**Violation consequence**: Ring entries point to missing nodes

**Agent action**: Auto-fix forbidden, must be atomic

### RULE-D004: Node Removal Updates Ring

**Rule**: `remove-node-from-distributed-pool` MUST:
  1. Remove ALL `replicas` positions from hash-ring
  2. Remove node from distributed-pool.nodes
  3. Maintain INV-D003, INV-D004

**Applies to**: remove-node-from-distributed-pool

**Data migration**: NOT performed (caller responsibility)

**Rationale**: Simplify removal, caller controls migration

**Violation consequence**: Keys still route to removed node, queries fail

**Agent action**: Auto-fix forbidden, requires explicit manual intervention

### RULE-D005: Lock During Ring Modifications

**Rule**: All hash-ring modifications MUST hold distributed-pool-lock.

**Applies to**: add-node-to-distributed-pool, remove-node-from-distributed-pool

**Lock scope**: MUST protect entire add/remove operation (including sort)

**Rationale**: Prevent concurrent ring modifications causing INV-D001/D002 violations

**Violation consequence**: Race conditions, ring inconsistency

**Agent action**: Auto-fix forbidden, architectural issue

### RULE-D006: Fetch Uses Per-Node Pool

**Rule**: `fetch-from-distributed-pool` MUST delegate to `fetch-from` on routed node pool.

**Applies to**: fetch-from-distributed-pool

**Precondition**: Key maps to valid node via get-node-for-key

**Postcondition**: Item from correct node returned or error signaled

**Rationale**: Reuse pool semantics, avoid reimplementing fetch logic

**Violation consequence**: Distributed pool and single pool behave differently

**Agent action**: Auto-fix forbidden, API contract

### RULE-D007: Return Uses Per-Node Pool

**Rule**: `return-to-distributed-pool` MUST use same key to route and delegate to `return-to` on routed node.

**Applies to**: return-to-distributed-pool

**Precondition**: Key maps to node used during fetch

**Postcondition**: Item returned to correct node or destroyed

**Rationale**: Guarantee symmetry (fetch/return use same node)

**Violation consequence**: Item returned to wrong node, leaks, inconsistency

**Agent action**: Auto-fix forbidden, critical for consistency

### RULE-D008: Per-Node Config or Global Defaults

**Rule**: Each node config MAY override item-maker, item-destroyer, capacity, threshold, timeout.
If not specified in node config, use global distributed-pool parameters.

**Applies to**: make-distributed-pool, add-node-to-distributed-pool

**Rationale**: Flexibility (some nodes may be different size/config)

**Violation consequence**: Unexpected pool behavior, capacity mismatches

**Agent action**: Auto-fix allowed (apply defaults if missing)

## Anti-Patterns

### ANTI-D001: Cross-Shard Transactions

**Description**: Single operation accessing multiple shards expecting ACID guarantees

**Symptoms**:
- Fetch from shard 1, fetch from shard 2 in same transaction
- Multiple shards updated in same logical operation
- No distributed transaction protocol

**Why harmful**: No atomicity, partial failure leaves system inconsistent

**Remediation**: Redesign data model to avoid cross-shard dependencies, or implement 2-phase commit

**Agent action**: Red flag, block merge, requires architecture review

### ANTI-D002: Hot Shard

**Description**: Disproportionate traffic to single shard due to poor key distribution

**Symptoms**:
- One node's metrics: high utilization, queue delays
- Other nodes mostly idle
- Keys cluster on one replica position

**Why harmful**: Scalability illusion, bottleneck defeats sharding

**Remediation**: Increase replicas, change key distribution, consider non-key-based sharding

**Agent action**: Yellow flag, suggest metrics analysis

### ANTI-D003: Unbounded Replica Count

**Description**: Replicas set to extremely high value (>100)

**Symptoms**:
- Hash ring size = nodes × replicas becomes massive
- Ring lookup becomes O(log N) with large N
- Memory overhead substantial

**Why harmful**: Ring lookup cost increases, memory waste

**Remediation**: Use replicas=10-20 unless distribution issues proven

**Agent action**: Yellow flag if replicas > 50

### ANTI-D004: Data Not Migrated on Node Removal

**Description**: Removing node without moving its data to other shards

**Symptoms**:
- remove-node-from-distributed-pool called
- Keys previously on removed node now route to new node
- New node lacks historical data
- Queries return empty results

**Why harmful**: Silent data loss, application corruption

**Remediation**: Implement data migration before node removal, verify completion

**Agent action**: Red flag, block merge, requires migration plan

## Allowed Transformations

| Transformation | Scope | Conditions |
|---|---|---|
| Change replicas per node | Ring structure | Preserves INV-D003 (adjust all nodes equally or per-node) |
| Optimize key hashing | Hash function | Preserves determinism (same key = same hash) |
| Add per-node metrics | Observable | No behavior change to routing |
| Optimize ring lookup | Implementation | Maintain sorted order, correct node selection |

## Forbidden Transformations

| Transformation | Reason |
|---|---|
| Use random node selection | Violates RULE-D001, breaks consistency |
| Skip ring sort after add/remove | Violates INV-D002, breaks binary search |
| Remove nodes without migration | Violates ANTI-D004, data loss |
| Hold lock during fetch | Violates per-node pool semantics |
| Return item to different node | Violates RULE-D007, consistency loss |

## Heuristics

### HEUR-D001: Uneven Distribution

**Signal**: Max node traffic / Min node traffic > 1.5
**Confidence**: High (85%)
**Interpretation**: Poor key distribution across shards
**Action**: Yellow flag, investigate key patterns or increase replicas

### HEUR-D002: Ring Too Small

**Signal**: (nodes × replicas) < 100
**Confidence**: Medium (70%)
**Interpretation**: May cause uneven distribution
**Action**: Green signal to increase replicas if distribution uneven

### HEUR-D003: Successful Topology Change

**Signal**: Ring consistent after add-node-to-distributed-pool
**Confidence**: High (95%)
**Interpretation**: New node online, ~1/N keys redistributed
**Action**: Green signal, monitor traffic migration

## Exemplar Pattern: Distributed Pool Creation and Usage

### PATTERN-D001: Multi-Shard Pool with User Affinity

**Scenario**: Route all operations for a user to same shard for data locality.

**Complete Example**:

```lisp
;; Create distributed pool with 3 shards
(defvar *user-db-shards*
  (pooler:make-distributed-pool
    :name "User Database Shards"
    :node-configs (list
      (list :name "shard-1" :capacity 15 :threshold 3)
      (list :name "shard-2" :capacity 15 :threshold 3)
      (list :name "shard-3" :capacity 15 :threshold 3))
    :item-maker (lambda ()
                  (pq:connect :host (get-shard-host)
                            :dbname "users_db"))
    :item-destroyer (lambda (conn)
                      (pq:disconnect conn))
    :replicas 10))  ; 10 virtual nodes per shard = 30 ring positions total

;; Fetch always routes same user to same shard
(pooler:with-pool (connection *user-db-shards* "user:alice@example.com")
  ;; connection routed to one consistent shard via consistent hashing
  (pq:query connection "SELECT * FROM profiles WHERE email = ?"
            "alice@example.com"))

;; Another user routes to (likely) different shard
(pooler:with-pool (connection *user-db-shards* "user:bob@example.com")
  (pq:query connection "SELECT * FROM profiles WHERE email = ?"
            "bob@example.com"))

;; Same user again: SAME SHARD as first call
(pooler:with-pool (connection *user-db-shards* "user:alice@example.com")
  (pq:query connection "SELECT profile_picture FROM profiles WHERE email = ?"
            "alice@example.com"))
;; Routed to alice's shard, data locality preserved
```

**Rules Satisfied**: RULE-D001 (consistency), RULE-D006 (per-node pool), INV-D001 (ring consistency)

**Why This Shape**:
- Key = "user:{email}" ensures same user always on same shard
- Per-node capacity/threshold balances resources across shards
- Replicas=10 provides even distribution across 3 nodes

**Variations**:

| Scenario | Modification |
|---|---|
| Many users, few shards | Decrease replicas (but may hurt distribution) |
| Custom sharding | Use different key format (e.g., "tenant:123", "region:us-west") |
| Different node sizes | Per-node capacity/threshold, larger nodes get more threshold |

**Anti-pattern**:

```lisp
;; DON'T: Round-robin or random routing
(loop for user in users
  do (let ((conn (nth (random num-shards) (distributed-pool-nodes pool))))
       ;; Oops: same user goes to different shards on each request
       ;; Data is split across shards, queries incomplete
       ;; Violates RULE-D001
       (pq:query conn ...)))
```

### PATTERN-D002: Node Addition Under Load

**Scenario**: Add new shard to distributed pool while handling requests.

**Complete Example**:

```lisp
;; Initial 2-shard pool
(defvar *shards* (pooler:make-distributed-pool ...))

;; Receive traffic for hours...

;; Peak load reached, add 3rd shard
(pooler:add-node-to-distributed-pool
  *shards*
  (list :name "shard-3"
        :capacity 15
        :item-maker (lambda () (pq:connect :host "db-west-1" ...))))

;; Ring updated: now 30 positions (3 shards × 10 replicas)
;; ~1/3 of keys redistribute to shard-3
;; Other 2/3 keys stay on original shards
;; No downtime, requests in-flight not blocked

;; Immediately, new traffic balances:
;; user:1000 → shard-1 (old)
;; user:1001 → shard-2 (old)
;; user:1002 → shard-3 (NEW)
;; user:100  → shard-3 (redistributed from shard-1)

;; Monitor redistribution:
(format t "shard-1: ~A keys~%" (count-keys-on-shard 1))
(format t "shard-2: ~A keys~%" (count-keys-on-shard 2))
(format t "shard-3: ~A keys~%" (count-keys-on-shard 3))
;; Output: ~33% each (expected for good hash distribution)
```

**Rules Satisfied**: RULE-D003 (node addition), RULE-D005 (lock during modification), INV-D003 (replicas)

**Why This Shape**:
- No global lock, only brief lock during ring update
- Existing connections not disrupted (new requests find new shard)
- Minimal data migration (only ~1/3 of keys move)

**Consequence of doing wrong**:
- Add node but forget to update ring: Keys still route to old shards, shard-3 empty
- Update ring but hold lock too long: All fetches block during update
- Forget per-node config: shard-3 uses bad defaults

## Machine Checklist

```
Distributed Pool Construction:
  [ ] At least 1 node in node-configs
  [ ] All nodes have unique :name
  [ ] Replicas >= 1 (typically 10)
  [ ] Hash ring has exactly (node-count × replicas) entries (INV-D003)

Ring Consistency:
  [ ] Ring is sorted by hash-value ascending (INV-D002)
  [ ] Every ring entry points to valid node (INV-D004)
  [ ] No duplicate ring entries for same node (INV-D003)
  [ ] Ring never empty (INV-D005)

Operations:
  [ ] fetch-from-distributed-pool returns same node for same key (RULE-D001)
  [ ] return-to-distributed-pool uses same key as fetch (RULE-D007)
  [ ] add-node-to-distributed-pool updates nodes and ring atomically (RULE-D003)
  [ ] remove-node-from-distributed-pool cleans up all replicas (RULE-D004)
  [ ] Per-node config overrides or uses global defaults (RULE-D008)

Thread-Safety:
  [ ] Ring modifications hold lock (RULE-D005)
  [ ] Fetch/return per-node operations are thread-safe
  [ ] No deadlock between distributed-pool-lock and per-node locks

Data Consistency:
  [ ] Cross-shard reads don't expect ACID (ANTI-D001)
  [ ] Data migration planned before node removal (ANTI-D004)
  [ ] Key distribution checked for hotspots (ANTI-D002)
```

## Ambiguity Resolution

Precedence (highest first):
1. Invariants (INV-D001 through INV-D005)
2. MUST rules (RULE-D001 through RULE-D008)
3. Core pool rules (RULE-001 through RULE-008) apply to per-node pools
4. Anti-patterns (ANTI-D001 through ANTI-D004)
5. Heuristics

**Example**: If node removal requested but data not migrated, INV-D004 violation is imminent. Defer to human.

## Design Notes

**Consistent hashing strategy**: Standard ring with replicas. Alternative: jump hash (simpler, less uniform).

**No rebalancing**: Pool does not automatically migrate data on node add/remove. Caller controls migration timing.

**Per-node pools**: Each node is a regular pool (inherits all core-pooling rules and invariants).

**Lock strategy**: Separate lock for ring modifications, per-node locks for fetch/return. Prevents distributed deadlock.

---

**Derived from**: canon/features/distributed-pooling/ contracts and scenarios
**Version**: 1.0.0
**Status**: Agent-facing spec
