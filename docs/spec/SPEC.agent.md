---
type: master-specification
version: 1.0.0
applies-to: [pooler]
contains: [CORE-POOLING, DISTRIBUTED-POOLING, ERROR-HANDLING, CONCURRENCY]
---

# Pooler: Master Agent Specification

Machine-oriented specification for Pooler resource pooling library. This is the normative source for agents implementing or verifying Pooler code.

## Quick Reference

### Scope

This specification covers:
- Pool creation and lifecycle (core pooling)
- Distributed pools with consistent hashing
- Error handling semantics
- Thread-safety and concurrency guarantees
- Platform abstraction (SBCL, Allegro, fallback)

### Not Covered

- User-facing API design (see human docs)
- Specific implementations (language bindings)
- Performance optimization (beyond specified lock discipline)
- Cloud deployment patterns

## Specification Organization

```
SPEC.agent.md (this file)
├── Core Pooling (CORE-POOLING.agent.md)
│   ├── Invariants: INV-001 through INV-005
│   ├── Rules: RULE-001 through RULE-008
│   ├── Anti-patterns: ANTI-001 through ANTI-004
│   └── Patterns: PATTERN-001, PATTERN-002
│
├── Distributed Pooling (DISTRIBUTED-POOLING.agent.md)
│   ├── Invariants: INV-D001 through INV-D005
│   ├── Rules: RULE-D001 through RULE-D008
│   ├── Anti-patterns: ANTI-D001 through ANTI-D004
│   └── Patterns: PATTERN-D001, PATTERN-D002
│
├── Error Handling (ERROR-HANDLING.agent.md)
│   ├── Rules: RULE-E001 through RULE-E004
│   ├── Anti-patterns: ANTI-E001 through ANTI-E003
│   └── Patterns: PATTERN-E001, PATTERN-E002
│
└── Concurrency (CONCURRENCY.agent.md)
    ├── Invariants: INV-C001 through INV-C004
    ├── Rules: RULE-C001 through RULE-C005
    ├── Anti-patterns: ANTI-C001 through ANTI-C004
    └── Patterns: PATTERN-C001, PATTERN-C002
```

## Cross-Document Rule References

### When implementing fetch-from:
- RULE-001: Grow when empty
- RULE-002: Check timeout, trigger pool-init if needed
- RULE-003: Return item or error
- RULE-008: Never return nil
- RULE-E001: Transform item-maker errors to pool-item-creation-error
- RULE-E002: Retry up to tries times
- RULE-C001: Hold lock during queue access only
- RULE-C002: Release lock before item-maker call
- RULE-C003: Atomic fetch-or-grow

### When implementing return-to:
- RULE-003: Store if room, destroy if full
- RULE-E003: Silently ignore destroyer errors
- RULE-C001: Hold lock during queue/capacity check
- RULE-C002: Release lock before destroyer call

### When implementing distributed pool fetch:
- RULE-D001: Route same key to same node (consistent)
- RULE-D006: Delegate to per-node pool fetch
- RULE-C004: No nested locks (per-node lock inside distributed lock OK)

## Invariant Summary

| ID | Scope | Verification | Consequence |
|---|---|---|---|
| INV-001 | Pool | `current-size ≤ capacity` | Capacity enforcement |
| INV-002 | Pool | `threshold ≤ capacity` | Configuration validity |
| INV-003 | Pool | All metrics ≥ 0 | No corruption |
| INV-004 | Pool | Items from item-maker | Type consistency |
| INV-005 | Pool | Destroyer on full return | No resource leak |
| INV-D001 | DistPool | Same key → same node | Data locality |
| INV-D002 | DistPool | Ring sorted | Binary search works |
| INV-D003 | DistPool | Replicas per node | Distribution valid |
| INV-D004 | DistPool | Ring entries exist | No dangling refs |
| INV-D005 | DistPool | Ring non-empty | No null node |
| INV-C001 | Concurrency | Queue consistent | No corruption |
| INV-C002 | Concurrency | Metrics monotonic | Stats reliable |
| INV-C003 | Concurrency | Capacity atomic | No overflow |
| INV-C004 | Concurrency | No starvation | All threads progress |

## Rule Categories

### MUST (Absolute) Rules

These are non-negotiable. Violation is a bug.

| Rule | Category | Consequence |
|---|---|---|
| RULE-001 | Core | Caller gets nil (bug) |
| RULE-002 | Core | Stale items returned |
| RULE-003 | Core | Resource leak |
| RULE-008 | Core | NullPointerException |
| RULE-E001 | Error | Unexpected exception types |
| RULE-E003 | Error | Pool operation crashes |
| RULE-D001 | DistPool | Data inconsistency |
| RULE-C001 | Concurrency | Queue corruption |
| RULE-C002 | Concurrency | Serialized access |

### SHOULD Rules

Strong recommendations. Exceptions require justification.

| Rule | Category | Typical Consequence |
|---|---|---|
| RULE-004 | Core | Explicit error interface |
| RULE-005 | Core | Misleading metrics |
| RULE-006 | Core | Thread-safety |
| RULE-E002 | Error | Resilience |
| RULE-D008 | DistPool | Flexibility |

### Implementation Freedom

These aspects are implementation-defined:

- Queue implementation (FIFO order required, structure flexible)
- Lock implementation (mutex required, specific type flexible)
- Hash function (deterministic required, algorithm flexible)
- Retry backoff strategy (exponential recommended, other ok)

## Test Verification Order

When verifying Pooler implementation, test in this order:

1. **Invariants First**
   - Test INV-001 through INV-005 (core pool invariants)
   - Verify each with multiple threads
   - Confirm no race conditions

2. **Rules Second**
   - RULE-001 (grow on empty)
   - RULE-002 (timeout handling)
   - RULE-003 (return behavior)
   - RULE-008 (non-nil return)

3. **Error Handling**
   - RULE-E001 (error transformation)
   - RULE-E002 (retry discipline)
   - RULE-E003 (destroyer resilience)

4. **Distributed (if applicable)**
   - Invariants INV-D001 through INV-D005
   - Rules RULE-D001 through RULE-D008
   - Consistency under topology changes

5. **Concurrency**
   - Multi-threaded stress tests
   - Lock contention measurement
   - Deadlock detection

6. **Anti-Patterns**
   - Verify none are present
   - Flag if found, block merge

## Allowed Variations

Implementations MAY vary in:

| Aspect | Flexibility |
|---|---|
| Queue implementation | Any FIFO data structure |
| Lock type | Mutex, semaphore, atomic operations |
| Hash function | Any deterministic hash (sxhash, md5, etc.) |
| Retry strategy | Any backoff (exponential, fixed, jittered) |
| Platform support | Subset of SBCL/Allegro/fallback |
| Performance optimization | Any change preserving all invariants |

## Required Artifacts

Complete Pooler implementation MUST provide:

1. **Core Pool Functions**
   - make-pool, fetch-from, return-to, with-pool, grow-pool, pool-init

2. **Distributed Pool Functions** (if supported)
   - make-distributed-pool, fetch-from-distributed-pool, return-to-distributed-pool
   - add-node-to-distributed-pool, remove-node-from-distributed-pool, get-node-for-key

3. **Error Types**
   - pool-error, pool-item-creation-error, pool-warning

4. **Test Suite**
   - Unit tests for all invariants
   - Concurrent stress tests
   - Error case coverage
   - Distributed pool topology tests

5. **Documentation**
   - Human-readable usage guide (see docs/README.md)
   - Agent-readable specification (this directory)

## Verification Checklist

Before declaring implementation complete:

```
Core Pooling:
  [ ] All invariants verified (INV-001 through INV-005)
  [ ] All MUST rules implemented (RULE-001, 002, 003, 008, C001, C002)
  [ ] All error rules implemented (RULE-E001, E002, E003)
  [ ] Pattern examples work correctly
  [ ] No anti-patterns present
  [ ] Thread-safe under concurrent load
  [ ] No resource leaks

Distributed Pooling (if supported):
  [ ] All invariants verified (INV-D001 through INV-D005)
  [ ] Consistent hashing works (same key → same node)
  [ ] Node addition redistributes ~1/N keys
  [ ] Node removal clean (INV-D003, D004 maintained)
  [ ] No deadlocks on topology changes
  [ ] Per-node pools work independently

Error Handling:
  [ ] item-maker errors → pool-item-creation-error
  [ ] item-destroyer errors silenced
  [ ] Retry logic respects tries parameter
  [ ] All error types properly signaled

Concurrency:
  [ ] No data corruption under multi-threaded access
  [ ] No deadlocks
  [ ] No thread starvation
  [ ] Metrics never corrupted

Documentation:
  [ ] Human docs present (tutorials, reference, how-tos)
  [ ] Agent specs present (this directory)
  [ ] Examples runnable
  [ ] All rules documented with rationale
```

## Normalization

When spec sections conflict:

1. **Invariants override everything** - If an invariant contradicts a rule, invariant wins
2. **MUST rules override SHOULD** - Absolute rules beat recommendations
3. **Core pooling rules override variants** - Base semantics non-negotiable
4. **Explicit spec over implicit assumption** - Stated rule beats inferred behavior

Example: If HEUR-001 suggests higher capacity but INV-002 requires threshold ≤ capacity, INV-002 wins.

## Change Control

Specification is versioned. Changes require:

1. Version number bump (major.minor.patch)
2. Rationale for change documented
3. All invariants verified with new rule
4. All implementations re-tested

Current version: 1.0.0 (stable)

## References

- **Consistent Hashing**: Karger, Lehman, Leighton, et al. "Consistent Hashing and Random Trees"
- **Thread-Safe Patterns**: Gorelick & Simonoff. "The Art of Multiprocessor Programming"
- **RFC 2119**: Key words for use in standards

## Meta

**Generated from**: Canon specifications (canon/features/core-pooling, distributed-pooling)
**Version**: 1.0.0
**Maturity**: Stable
**Audience**: Agent implementers, code verifiers, architecture reviewers
**Update frequency**: On spec changes only (backward-compatible additions ok)

---

## Navigation

Start with:
- **CORE-POOLING.agent.md** - Basic pool semantics
- **DISTRIBUTED-POOLING.agent.md** - Multi-node scaling
- **ERROR-HANDLING.agent.md** - Error semantics
- **CONCURRENCY.agent.md** - Thread-safety rules

Each spec is standalone but references this master document.
