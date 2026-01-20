---
type: decision
adr: "0003"
title: API Consolidation (fetch-from+ → fetch-from)
status: accepted
date: 2013-09-12
---

# ADR-0003: API Consolidation (fetch-from+ → fetch-from)

## Status

**Accepted** (implemented 2013-09-12)

**Breaking change**: Removed `fetch-from+` from public API

## Context

The original pooler API (pre-2013) had two separate fetch functions:

1. **`fetch-from pool`**: Fetch item, single attempt, no retry
   - If pool empty and growth fails → error immediately
   - Simple, but fragile (transient failures not handled)

2. **`fetch-from+ pool &key (tries 3)`**: Fetch with retry wrapper
   - Calls `fetch-from` up to N times
   - If any attempt succeeds → return item
   - Handles transient failures (race conditions, temporary resource exhaustion)

This dual-API created confusion:
- **Which function should users call?** Most wanted retry logic (fetch-from+)
- **Why two functions?** fetch-from+ was just a thin retry wrapper around fetch-from
- **Code duplication**: Users had to choose between fragile (fetch-from) or verbose (fetch-from+ :tries N)

Example of the confusion:
```lisp
;; Old API - which one do I use?
(fetch-from *pool*)        ;; Simple, but fragile
(fetch-from+ *pool*)       ;; More robust, but why separate function?
(fetch-from+ *pool* :tries 5)  ;; Custom retry, but verbose
```

At the same time, the timeout feature was being added (see ADR-0001). This was an opportunity to simplify the API.

## Decision

**Consolidate to single `fetch-from` function with built-in retry logic.**

Remove `fetch-from+` from public API. Make retry behavior the default in `fetch-from`.

### New API

```lisp
(fetch-from pool &key (tries 2))
```

- **Default behavior**: Retry up to 2 times (was 3 in old fetch-from+)
- **Explicit control**: Users can override with `:tries N`
- **Single function**: No confusion about which function to use

### Implementation

From commit e7591ac (2013-09-12):

1. Renamed old `fetch-from` → `fetch-from-aux` (internal helper, unexported)
2. Moved retry logic from `fetch-from+` into new `fetch-from`
3. Removed `fetch-from+` entirely
4. Updated exports in package.lisp

```lisp
;; Before: Two public functions
(export '(fetch-from fetch-from+ ...))

;; After: One public function
(export '(fetch-from ...))

;; fetch-from-aux is internal, not exported
```

### Migration

**Breaking change**: Code using `fetch-from+` must be updated.

```lisp
;; Old code
(fetch-from+ *pool*)            → (fetch-from *pool*)
(fetch-from+ *pool* :tries 5)   → (fetch-from *pool* :tries 5)

;; Old code (simple fetch-from, no retry)
(fetch-from *pool*)             → (fetch-from *pool* :tries 1)
```

Most users were already using `fetch-from+`, so migration was straightforward.

## Alternatives Considered

### Alternative 1: Keep Both Functions

**Approach**: Keep `fetch-from` (single attempt) and `fetch-from+` (retry wrapper) as separate functions.

**Pros**:
- Backwards compatible (no breaking change)
- Explicit choice: "+" suffix signals retry behavior
- Simple `fetch-from` for users who don't want retry

**Cons**:
- API confusion persists (which function to use?)
- Most users want retry logic (fetch-from+ becomes the "real" API)
- Code duplication (two exported functions, similar behavior)
- Teaching burden: documentation must explain both functions

**Why rejected**: Simplicity wins. Retry logic is generally desired, so make it the default. Users who want single-attempt can use `:tries 1`.

### Alternative 2: Rename to fetch-with-retry

**Approach**: Single function, but rename to make retry behavior explicit.

```lisp
(fetch-with-retry pool &key (tries 2))
```

**Pros**:
- Name documents behavior
- No confusion about retry semantics

**Cons**:
- Verbose (longer name)
- "Retry" is implementation detail, not user concern
- Name implies retry is optional/special, but it's the default behavior

**Why rejected**: `fetch-from` is clearer and more concise. The `:tries` parameter documents retry behavior sufficiently.

### Alternative 3: Keep fetch-from+, Deprecate fetch-from

**Approach**: Keep `fetch-from+`, deprecate old `fetch-from`, eventually rename fetch-from+ → fetch-from.

**Pros**:
- Smoother migration (deprecation period)
- Less breaking change impact

**Cons**:
- Two-phase migration (deprecate, then remove)
- Extended confusion period (two functions exist)
- More complex for new users learning the library

**Why rejected**: Clean break is better. Library was young (< 1 year old) with few users. Rip the band-aid off.

## Consequences

### Positive

1. **Simpler API**: One fetch function, not two
   - New users have one obvious choice
   - Documentation is simpler (explain one function)
   - Less cognitive load

2. **Retry by default**: Most common case (robust fetching) is the default
   - Users don't need to remember to use fetch-from+ for robustness
   - Fewer bugs from forgetting retry logic

3. **Explicit opt-out**: Users who want single-attempt use `:tries 1`
   - Still possible, but requires explicit choice
   - Discourages fragile code (default is robust)

4. **Cleaner exports**: One exported function, not two
   - Smaller API surface
   - Easier to maintain

5. **Internal helper preserved**: `fetch-from-aux` still exists for internal use
   - Separation of concerns: fetch-from handles retry, fetch-from-aux handles single attempt
   - Clean implementation (retry loop in one place)

### Negative

1. **Breaking change**: Code using `fetch-from+` must be updated
   - Mitigation: Search/replace `fetch-from+` → `fetch-from` is simple
   - Impact: Low (library was young, few users)

2. **Default tries changed**: 3 → 2
   - Old `fetch-from+` default was 3 attempts
   - New `fetch-from` default is 2 attempts
   - **Rationale**: 2 tries is sufficient for most cases (catches transient failures)
   - Users who need more can specify `:tries N`

3. **Behavior change for old fetch-from**: Now retries by default
   - Old `fetch-from` was single-attempt
   - New `fetch-from` retries (up to 2)
   - **Impact**: Low (most users were using fetch-from+, which already retried)
   - **Benefit**: More robust default behavior

4. **README outdated**: Still documents `fetch-from-aux` as public API
   - See historical observation obs-hist-001
   - README predates this change, never updated
   - Resolution: Update README to remove fetch-from-aux, document fetch-from

### Related Changes

This API consolidation happened in the same commit as the timeout feature (ADR-0001):
- Both changes simplified the pool lifecycle
- Both changes made the library more automatic (less user intervention)
- Timeout + retry = robust pool management

Other changes in same commit (e7591ac):
- Added `pool-timeout` field and timeout checking
- Changed default capacity from 4 → 40 (separate decision)
- Added `total-pool-inits` counter

## Usage Examples

### Basic Usage (No Change Needed)

```lisp
;; Old API (fetch-from+)
(fetch-from+ *pool*)

;; New API (same behavior)
(fetch-from *pool*)
```

### Custom Retry Count

```lisp
;; Old API
(fetch-from+ *pool* :tries 5)

;; New API (same)
(fetch-from *pool* :tries 5)
```

### Single Attempt (New in Consolidated API)

```lisp
;; Old API (used fetch-from, no retry)
(fetch-from *pool*)

;; New API (explicit single attempt)
(fetch-from *pool* :tries 1)
```

### With Macro (No Change)

```lisp
;; with-pool always used fetch-from internally
;; Now uses new fetch-from with retry (more robust)
(with-pool (item *pool*)
  (use-item item))
```

## Related Decisions

- **ADR-0001**: Pool Timeout with Lazy Checking (same commit, related simplification)

## Notes

### Commits

- e7591ac (2013-09-12): API consolidation + timeout feature

### Contributors

- Abhijit Rao (quasi): Original author

### Code Locations

- `fetch-from` implementation: pooler.lisp:91-108
- `fetch-from-aux` (internal helper): pooler.lisp:78-89
- Exports: package.lisp (fetch-from exported, fetch-from-aux NOT exported)

### Commit Message Quote

> "Slightly changed the interface. Sorry. the FETCH-FROM is the only function for fetch now. FETCH-FROM+ is removed."

The apology indicates awareness of breaking change, but confidence in the simplification.

### Historical Artifact

README.md still documents `fetch-from-aux` as public API (line 48-63):
```markdown
#### fetch-from-aux

`(fetch-from-aux pool)`
This fetches an item from pool.
```

This is a historical artifact. The README was written before the API consolidation and never updated. `fetch-from-aux` is internal (unexported) as of e7591ac.

**Resolution**: Update README to:
- Remove `fetch-from-aux` documentation
- Document `fetch-from` with `:tries` parameter
- Note that retry is default behavior

### Design Philosophy

This decision reflects a broader philosophy:
- **Simplicity over flexibility**: One function with parameter > two functions
- **Robust by default**: Retry is default, single-attempt requires opt-in
- **Clean breaks over gradual migration**: Better to break early than accumulate API debt

This philosophy carried through to later decisions (consistent hashing over simple modulo, lazy timeout over background threads).

### Future Considerations

- Could add `:retry-delay` parameter (wait between retries)
- Could make retry backoff exponential (not just linear retries)
- Could add per-attempt hooks (log each retry)

None of these are currently needed, so API stays simple.
