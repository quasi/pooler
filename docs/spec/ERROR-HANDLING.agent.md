---
type: specification
version: 1.0.0
applies-to: [error-handling, all]
requires: [core-pooling]
domain: error-semantics
---

# Error Handling Specification

Machine-oriented error classification, signaling, and recovery semantics.

## Terminology

- **pool-error**: Base condition for all pool-related errors
- **pool-item-creation-error**: Signaled when item-maker throws
- **pool-warning**: Signaled on non-critical events (pool-init)
- **transient error**: Temporary failure, retry likely succeeds
- **permanent error**: Failure due to bad configuration, retry won't help

## Error Hierarchy

```
error (Lisp base)
  └─ pool-error
       ├─ pool-item-creation-error (transient)
       ├─ pool-empty-error (legacy, unused)
       └─ pool-warning (advisory, not exception)
```

## Normative Rules

### RULE-E001: Item-Maker Errors → pool-item-creation-error

**Rule**: If item-maker throws ANY exception, MUST catch and signal `pool-item-creation-error`.

**Applies to**: `fetch-from`, `grow-pool` operations

**Precondition**: item-maker called

**Postcondition**: Exception transformed to pool-item-creation-error

**Rationale**: Uniform error type for pool vs. application errors

**Violation consequence**: Item-maker exceptions leak, unpredictable types

**Agent action**: Auto-fix forbidden, must be explicit

**Example**:

```lisp
;; item-maker throws socket error
(defun bad-maker ()
  (error "Connection refused"))

;; fetch-from MUST transform to pool-item-creation-error
(handler-case
  (pooler:fetch-from pool)
  (pooler::pool-item-creation-error (e)
    (format t "~A" e)))
;; NOT (handler-case ... (error (e) ...))
```

### RULE-E002: Retry on Item-Maker Failures

**Rule**: `fetch-from` MUST retry item creation up to `tries` times on pool-item-creation-error.

**Applies to**: `fetch-from` operation

**Parameter**: `tries` keyword argument (default 2)

**Behavior**:
1. Attempt 1: Fail → retry
2. Attempt 2: Fail → retry
3. Attempt 3: Fail → signal pool-error "Could not create item after N attempts"

**Rationale**: Transient failures (network blip) recover on retry

**Violation consequence**: Single failure aborts immediately, no resilience

**Agent action**: Auto-fix forbidden, must respect tries parameter

### RULE-E003: Item-Destroyer Errors Silenced

**Rule**: If item-destroyer throws, error MUST be caught and ignored (silently or logged).

**Applies to**: `return-to`, `pool-init` operations where destroyer called

**Implementation**: Wrap destroyer call in `ignore-errors`

**Rationale**: Destroyer errors must not propagate to caller (resource already gone)

**Violation consequence**: Destroyer error crashes pool operation, caller loses connection

**Agent action**: Auto-fix allowed (add ignore-errors wrapper)

**Example**:

```lisp
;; Good: silently handles destroyer failure
(ignore-errors (funcall item-destroyer item))

;; Bad: destroyer error propagates
(funcall item-destroyer item)
;; If destroyer fails: connection lost, exception to caller
```

### RULE-E004: Pool-Warning Signals on Re-Init

**Rule**: `pool-init` MUST signal `pool-warning` condition (advisory, not error).

**Applies to**: `pool-init` operation

**Rationale**: Visibility into pool refreshes (expected but worth noting)

**Behavior**: Caller can muffle-warning to suppress

**Violation consequence**: Silent pool-init, metrics lost

**Agent action**: Auto-fix allowed if missing

```lisp
;; Caller can suppress
(handler-bind
  ((pooler::pool-warning #'(lambda (e) (invoke-restart 'muffle-warning))))
  (pooler:fetch-from pool))
```

## Anti-Patterns

### ANTI-E001: Silent Failure Swallowing

**Description**: Catching and ignoring errors without logging or re-signaling

**Symptoms**:
- `(ignore-errors ...)` with no logging context
- Error silently disappears
- Debugger finds no trace
- Pool operations mysteriously fail

**Why harmful**: Undiagnosable failures, customer reports "it just broke"

**Remediation**: Log all errors before swallowing, or re-signal with context

**Agent action**: Yellow flag, suggest logging

### ANTI-E002: Unbounded Retry

**Description**: Retrying indefinitely without backoff or max attempts

**Symptoms**:
- Loop: (retry-forever)
- Transient failure becomes permanent hang
- CPU maxes out during failures
- No exponential backoff

**Why harmful**: Thread starvation, cascading failures, CPU waste

**Remediation**: Respect tries parameter, add exponential backoff

**Agent action**: Red flag, block merge

### ANTI-E003: Wrong Error Type Signaled

**Description**: Signaling generic error instead of pool-specific type

**Symptoms**:
- item-maker fails, signals error instead of pool-item-creation-error
- Pool corruption, caller can't distinguish errors
- Error handling code doesn't catch

**Why harmful**: Client error handling broken, can't distinguish transient vs. permanent

**Remediation**: Always use pool-error or subtypes

**Agent action**: Auto-fix forbidden, verify error type

## Exemplar Pattern: Error Handling in Operations

### PATTERN-E001: Safe Fetch with Error Context

**Scenario**: Caller needs to handle creation failures gracefully.

**Complete Example**:

```lisp
;; Pattern: Error handling with context
(defun fetch-with-context (pool operation-name)
  (handler-case
    (progn
      (format *log* "~A: Attempting fetch from ~A~%"
              operation-name (pooler::pool-name pool))
      (pooler:fetch-from pool :tries 3))
    (pooler::pool-item-creation-error (e)
      ;; Transient failure: log, return nil or fallback
      (format *log* "~A: Pool item creation failed: ~A~%"
              operation-name e)
      nil)
    (pooler::pool-error (e)
      ;; Pool error: configuration/system issue
      (format *log* "~A: CRITICAL: Pool error: ~A~%"
              operation-name e)
      (signal e))  ; Re-signal to caller
    (t (e)
      ;; Unexpected error: log and propagate
      (format *log* "~A: Unexpected error: ~A~%"
              operation-name e)
      (signal e))))

;; Usage: caller handles returned nil
(let ((connection (fetch-with-context pool "query-user")))
  (if connection
    (use-connection connection)
    (respond-with-error "Service unavailable")))
```

**Rules Satisfied**: RULE-E001 (pool-item-creation-error), RULE-E003 (destroyer errors)

**Why This Shape**:
- Distinguishes transient (creation failure) from permanent (pool error)
- Logs context (operation name) for debugging
- Respects tries parameter for retry discipline
- Caller decides: retry, fallback, or fail

**Variations**:

| Scenario | Modification |
|---|---|
| Metrics collection | Increment counter on pool-item-creation-error before returning nil |
| Circuit breaker | After N consecutive failures, stop fetching (return fallback immediately) |
| Monitoring | Send metric to observability system on error |

### PATTERN-E002: Item-Destroyer Error Resilience

**Scenario**: Item-destroyer may fail (connection already closed), must not crash cleanup.

**Complete Example**:

```lisp
;; Setup: destroyer that might fail
(defvar *pool*
  (pooler:make-pool
    :name "Resilient Pool"
    :item-maker (lambda () (open-connection))
    :item-destroyer (lambda (conn)
                      ;; May fail: connection already closed by DB
                      (close-connection conn))
    ...))

;; Pattern: Destroy with fallback and logging
(defun safe-destroy (pool item)
  (ignore-errors
    (funcall (pooler::pool-item-destroyer pool) item))
  ;; Item destroyed or error suppressed, continue
  ;; Never propagate destroyer errors to caller
  )

;; Alternative: With logging
(defun safe-destroy-logged (pool item)
  (handler-case
    (funcall (pooler::pool-item-destroyer pool) item)
    (t (e)
      (format *log* "Destroyer error (suppressed): ~A~%" e))))
```

**Rules Satisfied**: RULE-E003 (destroyer errors silenced), ANTI-E001 (logging)

**Why This Shape**:
- Destroyer errors never crash pool operations
- Caller not responsible for destroyer cleanup
- Logging provides visibility into destroyer failures

## Machine Checklist

```
Error Handling:
  [ ] All exceptions caught explicitly (never bare catch-all)
  [ ] pool-item-creation-error used for item-maker failures (RULE-E001)
  [ ] fetch-from respects tries parameter (RULE-E002)
  [ ] Item-destroyer errors suppressed, not propagated (RULE-E003)
  [ ] pool-init signals pool-warning (RULE-E004)

Error Avoidance:
  [ ] No unbounded retries (ANTI-E002)
  [ ] All errors logged with context (ANTI-E001)
  [ ] Correct error types signaled (ANTI-E003)

Recovery:
  [ ] Transient errors (creation) handled separately from permanent errors
  [ ] Caller can distinguish error types via handler-case
  [ ] Retry discipline respected (tries parameter)
```

## Error Classification Matrix

| Error Type | Cause | Transient? | Recoverable | Action |
|---|---|---|---|---|
| pool-item-creation-error | item-maker fails | Yes | Retry | RULE-E002 |
| Connection refused | DB unreachable | Yes | Retry with backoff | Caller |
| Authentication failed | Bad credentials | No | Fail, don't retry | Caller |
| Pool not initialized | nil pool passed | No | Fail | Caller |
| Capacity exceeded (full) | No items available | Yes | Wait or retry | Expected |
| Ring empty (distributed) | No nodes available | No | Fail | RULE-D005 |

## Ambiguity Resolution

Precedence (highest first):
1. Normative rules (RULE-E001 through RULE-E004)
2. Anti-patterns (avoid ANTI-E001 through ANTI-E003)
3. Caller's error handling preference

If ambiguity remains: Defer to human (never silently swallow with no logging).

---

**Derived from**: canon/core/foundation/ + error-handling feature
**Version**: 1.0.0
**Status**: Agent-facing spec
