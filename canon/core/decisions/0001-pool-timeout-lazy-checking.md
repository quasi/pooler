---
type: decision
adr: "0001"
title: Pool Timeout with Lazy Checking
status: accepted
date: 2013-09-12
---

# ADR-0001: Pool Timeout with Lazy Checking

## Status

**Accepted** (implemented 2013-09-12)

## Context

Database connections and other network resources can become stale if idle for too long. The server may close the connection, firewall rules may timeout, or the remote service may have restarted. When a pool manages such resources, fetching a stale item leads to errors.

The library needed a mechanism to:
1. Detect when a pool has been idle too long
2. Automatically refresh stale pool items
3. Avoid manual intervention by users

Two main approaches were considered:
1. **Active monitoring**: Background thread continuously checks pool age and proactively reinitializes
2. **Lazy checking**: Check pool age only when accessed (fetch operation)

## Decision

**Implement lazy timeout checking on fetch operations.**

The pool tracks `last-access` timestamp and checks it during `fetch-from`:
- If `(current-time - last-access) > timeout`, trigger `pool-init`
- `pool-init` destroys all items and regrows pool to threshold
- Default timeout: 300 seconds (5 minutes)
- Timeout is configurable per pool

### Implementation Details

From commit e7591ac (2013-09-12):

```lisp
;; In fetch-from
(when (> (- (get-universal-time) (pool-last-access pool))
         (pool-timeout pool))
  (pool-init pool))
```

All pool operations update `last-access`:
- `fetch-from` updates on every call
- `return-to` updates on successful return
- `grow-pool` updates when creating items

**Note**: There's an observed inconsistency where `return-to` does NOT update `last-access` when destroying items at capacity. This is tracked separately.

## Alternatives Considered

### Alternative 1: Background Thread Monitoring

**Approach**: Spawn a background thread per pool to continuously monitor age and reinitialize when timeout exceeded.

**Pros**:
- Proactive cleanup (pool always fresh when accessed)
- More precise timeout enforcement

**Cons**:
- Thread management complexity (one thread per pool)
- Cross-platform threading differences
- Resource overhead (threads consume memory/OS resources)
- Shutdown complexity (must kill threads when pools destroyed)
- Edge cases: what if pool is actively used when thread triggers reinit?

**Why rejected**: Complexity outweighs benefits. Most pools are accessed frequently enough that lazy checking is sufficient.

### Alternative 2: No Timeout Feature

**Approach**: Require users to manually call `pool-init` when they suspect stale items.

**Pros**:
- Simplest implementation
- No automatic behavior to reason about

**Cons**:
- User burden (must remember to reinit)
- Error-prone (forgotten reinits lead to stale connection errors)
- Defeats purpose of automatic pool management

**Why rejected**: Library should handle common failure modes automatically.

## Consequences

### Positive

1. **Simple implementation**: No background threads, no platform-specific threading code
2. **Low overhead**: Only checks timestamp on fetch (single integer comparison)
3. **Predictable**: Timeout only triggers during fetch, never mid-operation
4. **No threading complexity**: Works identically on single-threaded and multi-threaded implementations
5. **Automatic stale handling**: Users don't need to worry about stale connections

### Negative

1. **Lazy detection**: Pool isn't reinitialized until first fetch after timeout
   - If pool idle for 1 hour, timeout triggers on next fetch (not at timeout boundary)
   - This is acceptable: the pool isn't being used anyway

2. **All-or-nothing reinit**: Timeout destroys ALL items, even if only some are stale
   - Alternative would be per-item timestamps (much more complex)
   - Pool is likely all-stale if idle for timeout duration

3. **Non-configurable behavior**: Timeout always triggers `pool-init` (destroys all items)
   - Users cannot customize timeout behavior (e.g., validate items instead of destroying)
   - This keeps the API simple

### Related Changes

This decision was part of a larger API simplification (see ADR-0003):
- Removed `fetch-from+` function
- Consolidated to single `fetch-from` with retry logic
- Default capacity increased from 4 to 40

Initially timeout was hard-coded to 300 seconds. In 2015, commit da82a5e made it configurable via `:timeout` parameter to `make-pool` (community contribution by Elias Martenson).

## Related Decisions

- **ADR-0003**: API Consolidation (same commit introduced timeout and removed fetch-from+)

## Notes

### Commits

- e7591ac (2013-09-12): Initial timeout implementation
- da82a5e (2015-02-23): Made timeout configurable

### Contributors

- Abhijit Rao (quasi): Original implementation
- Elias Martenson (lokedhs): Configurability enhancement

### Code Locations

- Timeout check: pooler.lisp:98-100 (in `fetch-from`)
- Pool-init implementation: pooler.lisp:110-125
- Default timeout: pooler.lisp:52 (`:timeout 300`)
- Last-access updates: pooler.lisp:98, 106, 132

### Future Considerations

- Per-item timeout (complex, likely not needed)
- Configurable timeout behavior (validate vs destroy)
- Timeout metrics (how often reinit happens)
