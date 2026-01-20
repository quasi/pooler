# Tutorial: Core Pooling for Database Connections

In this tutorial, you'll learn how to create and manage a database connection pool to optimize your application's database access.

## What You'll Learn

- Create a pool sized for your workload
- Handle pool growth and capacity
- Use pools safely with the `with-pool` macro
- Monitor pool health
- Deal with stale connections

## Prerequisites

- Pooler loaded: `(ql:quickload :pooler)`
- A database library (we'll use CLSQL in examples, adapt for your library)
- Understanding of connection pooling basics

## Step 1: Understanding Pool Configuration

A pool has three key settings:

- **Capacity**: Maximum items (e.g., 10 connections max)
- **Threshold**: Minimum items to keep ready (e.g., 2 connections pre-created)
- **Timeout**: Refresh interval for stale items (e.g., 300 seconds)

These settings balance resource usage vs. responsiveness.

**Rule of thumb for capacity**:
- Small app (< 100 users): 5-10 connections
- Medium app (100-1000 users): 10-20 connections
- Large app (> 1000 users): 20-50 connections or use distributed pools

**Rule of thumb for threshold**:
- Usually 1/5 of capacity
- Prevents connection creation delays on startup
- Adjust based on your item creation time

## Step 2: Create a Database Pool

```lisp
(defvar *db-pool*
  (pooler:make-pool
    :name "Production Database"
    :capacity 15
    :threshold 3
    :timeout 300
    :item-maker (lambda ()
                  (clsql:connect
                    '("database.example.com" "mydb" "app_user" "password")
                    :database-type :postgresql
                    :auto-commit t))
    :item-destroyer (lambda (connection)
                      (ignore-errors
                        (clsql:disconnect :database connection)))))
```

**Explanation**:
- `:capacity 15` - Allow up to 15 concurrent connections
- `:threshold 3` - Start with 3 pre-created connections
- `:timeout 300` - Refresh the pool every 5 minutes
- `:item-maker` - Creates a PostgreSQL connection
- `:item-destroyer` - Cleanly closes connections

## Step 3: Fetch and Use Connections

The basic pattern is: fetch, use, return.

```lisp
(let ((connection (pooler:fetch-from *db-pool*)))
  (unwind-protect
    (progn
      ;; Use the connection
      (clsql:query "SELECT count(*) FROM users" :database connection)
      ;; Process results here
      )
    ;; Return the connection even if an error occurs
    (pooler:return-to *db-pool* connection)))
```

**Important**: Always return the connection, even on error. Use `unwind-protect` to guarantee this.

## Step 4: Use the Convenient Macro

Pooler provides `with-pool` to handle the fetch/return boilerplate:

```lisp
(pooler:with-pool (connection *db-pool*)
  ;; Connection is fetched here
  (let ((count (clsql:query "SELECT count(*) FROM users"
                            :database connection)))
    (print count)))
;; Connection is automatically returned here
```

This is much cleaner and equivalent to the previous example.

## Step 5: Handle Multiple Queries

You can use the same connection for multiple queries:

```lisp
(pooler:with-pool (connection *db-pool*)
  ;; All queries share the same connection
  (clsql:query "DELETE FROM logs WHERE created < NOW() - INTERVAL 1 DAY"
               :database connection)
  (let ((rows (clsql:query "SELECT COUNT(*) FROM logs"
                           :database connection)))
    (format t "Remaining logs: ~A~%" rows)))
```

**Benefit**: Reusing one connection for multiple operations is faster than fetching multiple times.

## Step 6: Monitor Pool Health

Track pool performance to catch problems early:

```lisp
(defun report-pool-health (pool)
  (let ((current (pooler::pool-current-size pool))
        (created (pooler::pool-total-created pool))
        (uses (pooler::pool-total-uses pool))
        (reinits (pooler::pool-total-pool-inits pool)))
    (format t "Pool: ~A~%"   (pooler::pool-name pool))
    (format t "  Available: ~A / ~A~%"
            current (pooler::pool-capacity pool))
    (format t "  Total created: ~A~%" created)
    (format t "  Total fetches: ~A~%" uses)
    (format t "  Pool reinits: ~A~%" reinits)))

;; Run this periodically
(report-pool-health *db-pool*)
;; Output:
;; Pool: Production Database
;;   Available: 3 / 15
;;   Total created: 7
;;   Total fetches: 42
;;   Pool reinits: 0
```

**What to look for**:
- `Pool reinits > 0`: Connections are timing out (good for stale detection)
- `Available = 0`: Pool is under capacity pressure (increase capacity)
- `Available = Capacity`: Pool is underutilized (decrease capacity)
- Large difference between created and uses: Items aren't being reused much

## Step 7: Handle Stale Connections

Databases sometimes close idle connections after a timeout. Pooler handles this automatically via the `timeout` parameter:

```lisp
;; This pool refreshes every 5 minutes
(defvar *db-pool*
  (pooler:make-pool
    :name "Database"
    :capacity 10
    :threshold 2
    :timeout 300  ; Refresh every 5 minutes
    :item-maker (lambda () (connect-to-db))
    :item-destroyer (lambda (conn) (close-connection conn))))
```

**How it works**:
1. When you fetch, Pooler checks if the pool is "old" (idle > 300 seconds)
2. If old, it destroys all items and recreates them
3. This ensures all connections are fresh

**Adjust timeout based on your database**:
- MySQL: Usually 8 hours default (28800 seconds) - set timeout to 7200 seconds (2 hours)
- PostgreSQL: Usually 10 minutes default - set timeout to 300 seconds (5 minutes)
- Check your database documentation

## Step 8: Handle Connection Errors

Connection creation might fail. Here's how Pooler handles it:

```lisp
(handler-case
  (pooler:with-pool (connection *db-pool*)
    (clsql:query "SELECT 1" :database connection))
  (pooler::pool-item-creation-error (e)
    ;; All connection creation attempts failed
    (format t "Database unavailable: ~A~%" e))
  (clsql:database-error (e)
    ;; Query failed (connection succeeded but query is bad)
    (format t "Query failed: ~A~%" e)))
```

**Error types**:
- `pool-item-creation-error`: Item maker function failed
- Database errors: Connection succeeded, but query/operation failed

## Step 9: Multiple Pools for Different Databases

You might connect to multiple databases:

```lisp
(defvar *user-db-pool*
  (pooler:make-pool
    :name "User Database"
    :capacity 10
    :item-maker (lambda () (connect-to-db :database "users_db"))
    :item-destroyer (lambda (c) (disconnect-db c))))

(defvar *logs-db-pool*
  (pooler:make-pool
    :name "Logs Database"
    :capacity 5  ; Smaller for logs (less critical)
    :item-maker (lambda () (connect-to-db :database "logs_db"))
    :item-destroyer (lambda (c) (disconnect-db c))))

;; Use both
(pooler:with-pool (user-conn *user-db-pool*)
  (pooler:with-pool (log-conn *logs-db-pool*)
    ;; Query both databases
    (let ((user (get-user user-conn "alice")))
      (log-event log-conn (format nil "User loaded: ~A" user)))))
```

## Common Mistakes to Avoid

❌ **Don't forget to return connections**
```lisp
;; BAD
(let ((conn (pooler:fetch-from *db-pool*)))
  (query conn "SELECT 1"))
;; Connection leaked!
```

✓ **Use with-pool or unwind-protect**
```lisp
;; GOOD
(pooler:with-pool (conn *db-pool*)
  (query conn "SELECT 1"))
;; Connection always returned
```

---

❌ **Don't use a pool from multiple threads without care**
Pooler's pools are thread-safe, but:
- Only fetch/return from one thread, or
- Use distributed pools for multi-threaded access

✓ **Use distributed pools for concurrent access**
```lisp
;; GOOD for multi-threaded apps
(pooler:with-pool (conn *distributed-pool* user-id)
  (query conn "SELECT * FROM user_data"))
```

## What's Next?

- [Distributed Pooling Tutorial](distributed-pooling.md) - Scale across multiple nodes
- [How-to: Set Up Database Pool](../how-to/setup-database-pool.md) - Production setup guide
- [API Reference](../reference/api.md) - Complete function list
- [Architecture Guide](../architecture.md) - Understand the design
