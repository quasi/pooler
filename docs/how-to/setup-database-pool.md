# How-to: Set Up a Database Connection Pool for Production

This guide walks through setting up Pooler for a production web application.

## Prerequisites

- Pooler installed and loaded
- A database library (CLSQL, postmodern, etc.)
- Understanding of your database's connection limits
- Target number of concurrent users

## Step 1: Determine Pool Size

**Find your database's max connections:**

```sql
-- PostgreSQL
SHOW max_connections;
-- Output: 100 (default on many systems)

-- MySQL
SHOW VARIABLES LIKE 'max_connections';
-- Output: 151 (or your configured value)
```

**Plan your pool capacity:**
- Reserve 10% for other connections (monitoring, backups, etc.)
- Divide remaining by expected connection pools

Example with PostgreSQL (100 max connections):
```
100 - 10 (reserved) = 90 available
90 / 2 pools (read-heavy + write-heavy) = 45 per pool

But usually: 10-20 per pool is sufficient
```

**Conservative sizing:**
```lisp
(defvar *db-pool*
  (pooler:make-pool
    :capacity 15          ; Conservative: 15 connections
    :threshold 3          ; Keep 3 warm
    :timeout 600))        ; Refresh every 10 minutes
```

## Step 2: Handle Connection String Secrets

**Don't hardcode credentials!**

❌ Bad:
```lisp
(defvar *db-pool*
  (pooler:make-pool
    :item-maker (lambda ()
                  (connect :password "secret123"))))
```

✓ Good:
```lisp
(defvar *db-pool*
  (let ((password (uiop:getenv "DB_PASSWORD")))
    (pooler:make-pool
      :item-maker (lambda ()
                    (connect :password password)))))
```

Or use a secrets management system (Vault, sealed env vars, etc).

## Step 3: Create Connection Wrapper

For easier pool management, create a wrapper:

```lisp
(defvar *db-pool* nil)

(defun init-db-pool (&key host database user password (pool-size 15))
  "Initialize the database connection pool."
  (setf *db-pool*
        (pooler:make-pool
          :name (format nil "PostgreSQL ~A@~A" database host)
          :capacity pool-size
          :threshold (max 1 (floor pool-size 5))
          :timeout 600
          :item-maker (lambda ()
                        (clsql:connect
                          (list host database user password)
                          :database-type :postgresql
                          :auto-commit t))
          :item-destroyer (lambda (connection)
                            (ignore-errors
                              (clsql:disconnect :database connection))))))

(defun with-db (form)
  "Execute a database operation using a pooled connection."
  (pooler:with-pool (connection *db-pool*)
    (funcall form connection)))

(defmacro db (&body body)
  "Convenience macro for database operations."
  `(with-db (lambda (connection)
              (let ((database connection))
                ,@body))))

;; Usage:
(db (clsql:query "SELECT 1" :database database))
```

## Step 4: Initialize on Application Startup

In your application's initialization code:

```lisp
(defun startup ()
  ;; ... other startup code ...

  ;; Initialize database pool
  (init-db-pool
    :host (or (uiop:getenv "DB_HOST") "localhost")
    :database (or (uiop:getenv "DB_NAME") "myapp")
    :user (or (uiop:getenv "DB_USER") "app_user")
    :password (uiop:getenv "DB_PASSWORD")
    :pool-size (parse-integer (or (uiop:getenv "POOL_SIZE") "15")))

  (format t "Database pool initialized~%"))

;; Call at application startup
(startup)
```

## Step 5: Add Health Check

Create a health check to verify the database is accessible:

```lisp
(defun db-health-check ()
  "Check if database is accessible."
  (handler-case
    (db (clsql:query "SELECT 1"))
    (t (e)
      (format t "Database health check failed: ~A~%" e)
      nil)))

;; Use in your health endpoint:
(defroute health ("/health" :method :get)
  (if (db-health-check)
      (respond-json :status "healthy")
      (respond-json :status "unhealthy" :code 500)))
```

## Step 6: Monitor Pool Metrics

Add monitoring for pool health:

```lisp
(defun pool-metrics ()
  "Return pool metrics for monitoring."
  (list
    :name (pooler::pool-name *db-pool*)
    :available (pooler::pool-current-size *db-pool*)
    :capacity (pooler::pool-capacity *db-pool*)
    :total_created (pooler::pool-total-created *db-pool*)
    :total_uses (pooler::pool-total-uses *db-pool*)
    :reinits (pooler::pool-total-pool-inits *db-pool*)))

;; Log periodically (every minute):
(defun report-pool-metrics ()
  (let ((metrics (pool-metrics)))
    (format t "Pool metrics: ~{~A=~A ~}~%" metrics)))

;; In your monitoring system:
(defroute metrics ("/metrics" :method :get)
  (respond-json (pool-metrics)))
```

## Step 7: Handle Errors Gracefully

```lisp
(defmacro db-safe (&body body)
  "Execute database operation with error handling."
  `(handler-case
     (progn ,@body)
     (pooler::pool-error (e)
       (format t "Database pool error: ~A~%" e)
       nil)
     (clsql:database-error (e)
       (format t "Database error: ~A~%" e)
       nil)
     (t (e)
       (format t "Unexpected error: ~A~%" e)
       nil)))

;; Usage:
(db-safe
  (db (clsql:query "SELECT * FROM users")))
```

## Step 8: Graceful Shutdown

On application shutdown, clean up the pool:

```lisp
(defun shutdown ()
  "Clean shutdown of the application."
  (when *db-pool*
    (format t "Shutting down database pool...~%")
    ;; Connections are cleaned up when pool is GC'd
    ;; No explicit cleanup needed
    (setf *db-pool* nil))
  (format t "Shutdown complete~%"))

;; Register with your application framework
;; (e.g., Hunchentoot, Woo, etc.)
```

## Step 9: Test the Pool

Quick test to verify setup:

```lisp
(defun test-pool ()
  (format t "Testing database pool...~%")

  ;; Test 1: Basic fetch/return
  (db-safe
    (format t "✓ Basic query works~%"))

  ;; Test 2: Check pool metrics
  (let ((metrics (pool-metrics)))
    (format t "✓ Pool metrics: ~A/~A available~%"
            (getf metrics :available)
            (getf metrics :capacity)))

  ;; Test 3: Multiple concurrent queries (simple version)
  (dotimes (i 3)
    (db-safe
      (db (clsql:query "SELECT 1"))))
  (format t "✓ Multiple queries work~%")

  (format t "All tests passed!~%"))

(test-pool)
```

## Step 10: Environment Configuration

Create an example `.env.example`:

```bash
# Database Configuration
DB_HOST=localhost
DB_NAME=myapp_production
DB_USER=app_user
DB_PASSWORD=change_me_in_production
POOL_SIZE=15

# Optional
DB_PORT=5432
DB_TIMEOUT=600
```

Document in your README:

```markdown
## Configuration

Set environment variables:

```bash
export DB_HOST=database.example.com
export DB_NAME=production_db
export DB_USER=app_user
export DB_PASSWORD=$(aws secretsmanager get-secret-value --secret-id db-password)
export POOL_SIZE=20
```
```

## Tuning Tips

### Pool Too Small (All connections busy)
**Symptom**: Requests waiting for connections
**Fix**: Increase `:capacity` and `:threshold`

### Pool Too Large (Wasted resources)
**Symptom**: Most connections idle, high memory usage
**Fix**: Decrease `:capacity`

### Frequent Re-initialization (High "reinits" metric)
**Symptom**: Database disconnects frequently
**Fix**: Increase `:timeout` or check database idle timeout settings

### Connection Creation Failures
**Symptom**: Lots of `pool-item-creation-error`
**Fix**: Check database credentials, network, database availability

## References

- [PostgreSQL Connection Management](https://www.postgresql.org/docs/current/runtime-config-connection.html)
- [MySQL Connection Pool Sizing](https://dev.mysql.com/doc/refman/8.0/en/connection-management.html)
- [API Reference](../reference/api.md) - Complete pool API
- [Core Pooling Tutorial](../tutorials/core-pooling.md) - More details
