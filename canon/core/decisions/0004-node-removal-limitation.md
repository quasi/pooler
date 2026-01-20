---
type: decision
adr: "0004"
title: Node Removal Without Item Migration
status: accepted
date: 2025-06-09
---

# ADR-0004: Node Removal Without Item Migration

## Status

**Accepted** (implemented 2025-06-09)

**Known Limitation**: Documented in code and contracts

## Context

The distributed pool feature enables dynamic topology changes: nodes can be added or removed at runtime. When a node is added, the hash ring is updated and some keys reroute to the new node. This is straightforward—new keys fetch from the new node, old keys continue fetching from existing nodes.

**Node removal is more complex.** When a node is removed:

1. **Hash ring update**: Remove node's virtual nodes from ring ✓ (simple)
2. **Key rerouting**: Keys that mapped to removed node now map elsewhere ✓ (automatic via consistent hashing)
3. **Items in removed node's pool**: What happens to them? ⚠️ (problem)

The removed node's pool may contain active items (connections, workers, etc.). These items are:
- **Not destroyed**: The pool object still exists (just no longer in distributed pool's node list)
- **Not migrated**: Items aren't moved to other nodes
- **Orphaned**: Items sit in removed pool, unreachable via distributed pool API

This raises questions:
- Should items be automatically destroyed?
- Should items be migrated to other nodes?
- Should removal be blocked if pool has active items?
- Should draining be automatic (wait for items to return, then remove)?

## Decision

**Node removal does NOT handle item draining or migration.**

The implementation:
1. Removes node from distributed pool's node list
2. Removes all virtual nodes for that node from hash ring
3. Returns the modified distributed pool
4. **Does nothing with items in the removed pool**

This is explicitly documented in the code (pooler.lisp:229-231):

```lisp
;; NOTE: This version does not handle draining items from the pool being removed.
;; Items in the removed pool will be orphaned until its pool-init or manual cleanup.
```

### User Responsibility

Users must handle item lifecycle if they care about orphaned items:

**Option 1: Manual drain before removal**
```lisp
(let ((node (find "node2" (distributed-pool-nodes *pool*)
                  :key #'pool-name :test #'string=)))
  ;; Wait for items to return
  (loop while (> (pool-current-size node) 0)
        do (sleep 1))
  ;; Now remove
  (remove-node-from-distributed-pool *pool* "node2"))
```

**Option 2: Force pool-init (destroys items)**
```lisp
(let ((node (find "node2" (distributed-pool-nodes *pool*)
                  :key #'pool-name :test #'string=)))
  ;; Destroy all items
  (pool-init node)
  ;; Now remove
  (remove-node-from-distributed-pool *pool* "node2"))
```

**Option 3: Accept orphaning**
```lisp
;; Just remove, items will be GC'd eventually
(remove-node-from-distributed-pool *pool* "old-node")
```

## Alternatives Considered

### Alternative 1: Automatic Item Draining

**Approach**: Block `remove-node-from-distributed-pool` until all items returned to pool.

```lisp
(defun remove-node-with-drain (dist-pool node-name)
  (let ((node (find-node-by-name dist-pool node-name)))
    ;; Wait for all items to return
    (loop while (> (pool-current-size node) 0)
          do (sleep 0.1))
    ;; Then remove
    (remove-node-from-ring dist-pool node-name)))
```

**Pros**:
- No orphaned items
- Safe removal (all items properly returned before node removed)
- No resource leaks

**Cons**:
- **Blocking operation**: Could wait indefinitely if items never returned
  - What if item is stuck in user code?
  - What if item-destroyer is slow?
- **Timeout complexity**: Need timeout parameter, what happens on timeout?
- **User control lost**: User can't force immediate removal
- **Distributed pool lock held**: Blocks all operations while draining

**Why rejected**: Blocking indefinitely is unacceptable. Could hang the application if items stuck. Timeout adds complexity (what's the right timeout? what happens on timeout?).

### Alternative 2: Automatic Item Migration

**Approach**: Move items from removed pool to other nodes.

```lisp
(defun remove-node-with-migration (dist-pool node-name)
  (let ((node (find-node-by-name dist-pool node-name))
        (other-nodes (remove node (distributed-pool-nodes dist-pool))))
    ;; Drain items from removed pool
    (loop while (> (pool-current-size node) 0)
          for item = (dequeue (pool-queue node))
          for target-node = (pick-random other-nodes)
          do (enqueue (pool-queue target-node) item))
    ;; Update sizes
    ;; Then remove from ring
    (remove-node-from-ring dist-pool node-name)))
```

**Pros**:
- No orphaned items
- No resource waste (items reused)
- Smooth capacity reduction

**Cons**:
- **Breaks key-to-node mapping**: Items migrated to random nodes, not consistent with key hashing
  - Item fetched from nodeA, returned to nodeB → wrong node!
  - Violates deterministic routing property
- **Complex accounting**: Must update current-size, total-created counters across pools
- **What about in-flight items?** Items currently fetched aren't in any pool
  - User might return to removed node (if they hold reference)
  - Need to track "removed but accepting returns" state
- **Pool configuration differences**: Nodes may have different capacity, threshold, item-maker
  - Can't just move items between pools with different configurations

**Why rejected**: Breaks fundamental distributed pool invariant (key → node mapping). Would corrupt pool state. Complexity far exceeds benefit.

### Alternative 3: Graceful Removal with Cooldown

**Approach**: Two-phase removal: mark node as "draining", then remove after cooldown.

```lisp
(defun start-node-removal (dist-pool node-name &key (cooldown 30))
  (mark-node-as-draining dist-pool node-name)
  ;; Keys still map to draining node (can return items)
  ;; But fetch-from won't grow draining pool
  (sleep cooldown)
  ;; After cooldown, assume items returned
  (remove-node-from-distributed-pool dist-pool node-name))
```

**Pros**:
- Allows in-flight items to return
- Bounded wait time (cooldown parameter)
- More graceful than immediate removal

**Cons**:
- **Cooldown is arbitrary**: How long is enough? Too short = orphaned items, too long = slow removal
- **Requires new state**: "draining" mode for pools
- **Complex API**: Two-step process (start-removal, then actual removal)
- **Still orphans items**: If cooldown expires before all items returned
- **Fetch behavior unclear**: What if fetch called on draining pool?

**Why rejected**: Adds significant complexity (new pool state, multi-phase API) without solving the fundamental problem (items may still be orphaned).

### Alternative 4: Block Removal if Pool Non-Empty

**Approach**: Error if attempting to remove node with items.

```lisp
(defun remove-node-from-distributed-pool (dist-pool node-name)
  (let ((node (find-node-by-name dist-pool node-name)))
    (when (> (pool-current-size node) 0)
      (error 'pool-error
             :message "Cannot remove node with active items. Drain first."))
    (remove-node-from-ring dist-pool node-name)))
```

**Pros**:
- Safe (prevents orphaning)
- Forces user to think about item lifecycle
- Simple implementation

**Cons**:
- **User burden**: User must manually drain before removal
- **Not actually different from current solution**: Just moves responsibility to user (same as "Option 1: Manual drain" above)
- **Less flexible**: Can't force removal even if user doesn't care about items

**Why rejected**: Doesn't add value over documented limitation + manual drain option. Current solution allows both forced removal AND manual drain, this only allows manual drain.

## Consequences

### Positive

1. **Simple implementation**: Just remove from ring, that's it
   - No blocking, no state tracking, no migration logic
   - 22 lines of code (pooler.lisp:215-236)

2. **Predictable behavior**: Removal is immediate, always succeeds
   - No hanging, no timeouts, no "draining" state
   - User knows exactly what happens (node removed from ring, items orphaned)

3. **User control**: User chooses what to do with items
   - Force remove (don't care about items)
   - Manual drain (wait for items to return)
   - Pool-init (destroy items immediately)
   - Flexible for different use cases

4. **No lock held during drain**: Removal is fast, doesn't block other operations
   - If we waited for drain, distributed pool would be locked the entire time

5. **Consistent with add-node**: Adding node is simple (just update ring), removing is equally simple
   - Symmetric API (add and remove both focus on topology, not item lifecycle)

### Negative

1. **Orphaned items**: Items in removed pool are unreachable via distributed pool API
   - **Consequence**: Resource leak if user doesn't handle cleanup
   - **Mitigation**: Documented in code, contracts, and this ADR
   - **Mitigation**: Examples show manual drain patterns

2. **User responsibility**: User must think about item lifecycle
   - **Pro**: User has full control
   - **Con**: Easy to forget (especially for new users)
   - **Mitigation**: Documentation clearly states limitation

3. **In-flight items**: Items currently fetched may be returned to removed pool
   - If user holds reference to old node (via `get-node-for-key` before removal)
   - Item still returns successfully, but now in orphaned pool
   - **This is actually safe**: Item returned, just not reusable

4. **No graceful shutdown**: Can't smoothly drain node before removal
   - Must implement draining logic manually if needed
   - **Mitigation**: Examples show drain patterns

### Risk Assessment

**Low risk for most use cases:**

1. **Scale-down scenario**: Removing excess capacity
   - Wait for low traffic period
   - Manually drain (items return naturally)
   - Then remove node
   - Orphaned items unlikely (pool was idle)

2. **Maintenance scenario**: Remove node for upgrade
   - Force pool-init (destroy all items)
   - Remove node
   - Upgrade
   - Add node back
   - No orphaning (items destroyed intentionally)

3. **Emergency removal**: Node hardware failed
   - Force remove (node is dead anyway)
   - Items on dead node are lost
   - Accept orphaning (can't recover dead node's items)

**Higher risk scenarios:**

1. **High-traffic removal**: Removing node under heavy load
   - Many in-flight items
   - Items may be returned to orphaned pool
   - **Mitigation**: Drain first, or wait for low traffic

2. **Long-lived items**: Items fetched and held for extended periods
   - Hard to know when pool is drained
   - **Mitigation**: Application design (don't hold pool items long)

## Future Work

Potential enhancements (not implemented):

1. **Optional automatic draining**: Add `:drain t` parameter
   ```lisp
   (remove-node-from-distributed-pool *pool* "node2" :drain t :timeout 30)
   ```
   - Block until pool drained or timeout
   - Signal timeout error if timeout exceeded
   - **Benefit**: Convenience for users who want automatic drain
   - **Cost**: Complexity of timeout handling, blocking behavior

2. **Item migration with key tracking**: Track which key each item came from
   ```lisp
   ;; Each pool item tagged with originating key
   (make-tagged-item :item conn :key "user:alice")
   ```
   - On removal, migrate items to correct node based on key
   - **Benefit**: No orphaning, correct key→node mapping maintained
   - **Cost**: Significant complexity (item tagging, key→item mapping)

3. **Removal state tracking**: Expose pool state (active, draining, removed)
   ```lisp
   (pool-state node) → :active | :draining | :removed
   ```
   - Allow users to query removal state
   - Implement graceful removal workflow
   - **Benefit**: More observability
   - **Cost**: State management complexity

4. **Removal callbacks**: Allow user-provided drain function
   ```lisp
   (remove-node-from-distributed-pool
     *pool* "node2"
     :on-remove (lambda (node) (drain-pool node)))
   ```
   - User controls draining logic
   - **Benefit**: Flexibility
   - **Cost**: Callback management, error handling

**None of these are planned**. Current solution is sufficient for intended use cases.

## Related Decisions

- **ADR-0002**: Consistent Hashing (explains key redistribution on removal)

## Related Contracts

- `remove-node-from-distributed-pool` - Documents this limitation explicitly
- `add-node-to-distributed-pool` - Symmetric operation (no item migration on add either)

## Notes

### Commits

- cead997 (2025-06-09): Initial implementation with documented limitation

### Contributors

- google-labs-jules[bot]: AI agent implementation

### Code Locations

- Implementation: pooler.lisp:215-236
- Code comment: pooler.lisp:229-231 (documents limitation)
- Contract: canon/features/distributed-pooling/contracts/remove-node-from-distributed-pool.md

### Code Comment Quote

```lisp
;; NOTE: This version does not handle draining items from the pool being removed.
;; Items in the removed pool will be orphaned until its pool-init or manual cleanup.
```

This comment clearly documents the design decision in the code itself.

### Documentation Coverage

This limitation is documented in:
1. Code comments (pooler.lisp:229-231)
2. Contract specification (remove-node-from-distributed-pool.md)
3. This ADR (comprehensive rationale)

Users have multiple sources to discover this limitation.

### Design Philosophy

This decision reflects the library's philosophy:
- **Simplicity over magic**: No hidden automatic behavior
- **User control over automation**: User decides how to handle edge cases
- **Explicit over implicit**: Documented limitation better than complex automatic handling
- **MVP approach**: Ship simple, working solution; enhance later if needed

Same philosophy seen in:
- Lazy timeout checking (ADR-0001): No background threads
- API consolidation (ADR-0003): One function, not two
- Consistent hashing (ADR-0002): Standard algorithm, not custom

### Real-World Impact

In practice, this limitation is minor:
- Node removal is infrequent (scaling down, maintenance)
- Manual drain is straightforward (wait for pool size = 0)
- Most pools have fast item turnover (connections returned quickly)

The simplicity gained outweighs the manual effort for occasional node removal.

### Testing

Tests in `tests/test-pooler.lisp` verify:
- Node removal updates hash ring correctly
- Keys reroute to other nodes after removal
- Removal succeeds regardless of pool state

Tests do NOT verify item draining (intentionally, since it's not implemented).

### Example: Production Use Case

```lisp
;; Scale down from 10 nodes to 8 nodes
(defun scale-down-distributed-pool (dist-pool target-count)
  (let ((current-nodes (distributed-pool-nodes dist-pool)))
    (loop for node in current-nodes
          for i from 1
          until (<= (length (distributed-pool-nodes dist-pool)) target-count)
          do (progn
               ;; Log removal
               (format t "Draining node ~A...~%" (pool-name node))
               ;; Wait for drain (with timeout)
               (loop repeat 60
                     while (> (pool-current-size node) 0)
                     do (sleep 1))
               ;; Warn if not fully drained
               (when (> (pool-current-size node) 0)
                 (warn "Node ~A not fully drained after 60s (size: ~A)"
                       (pool-name node)
                       (pool-current-size node)))
               ;; Remove anyway
               (remove-node-from-distributed-pool dist-pool (pool-name node))
               (format t "Removed node ~A~%" (pool-name node))))))
```

This example shows practical usage: attempt drain with timeout, warn if not drained, remove anyway. User has full control and visibility.
