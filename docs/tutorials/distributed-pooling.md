# Tutorial: Distributed Pooling with Consistent Hashing

In this tutorial, you'll learn how to distribute a pool across multiple backend nodes for horizontal scaling.

## What You'll Learn

- Create a distributed pool with multiple nodes
- Understand consistent hashing and why it matters
- Route requests consistently to the same node
- Add and remove nodes dynamically
- Scale beyond a single database server

## Prerequisites

- Pooler loaded: `(ql:quickload :pooler)`
- Completed [Core Pooling Tutorial](core-pooling.md)
- Multiple backend servers (or multiple databases for this demo)
- Understanding of sharding concepts

## The Problem Distributed Pools Solve

A single database server has limits:
- Maximum connections (usually 100-1000)
- Maximum query throughput
- Data size limits

When you outgrow a single server, you need to distribute requests across multiple servers. But you need **consistency**: user 123's data should always go to the same server, or you'll have synchronization problems.

**Distributed pools solve this** with consistent hashing: the same key always routes to the same node, while distributing evenly across servers.

## Step 1: Understand Consistent Hashing

Imagine you have 3 database shards:

```
Key: "user:123"  → Hash value: 847     → Shard 1
Key: "user:456"  → Hash value: 12094   → Shard 2
Key: "user:789"  → Hash value: 4512    → Shard 3
Key: "user:999"  → Hash value: 847     → Shard 1  (same as user:123!)
```

**Key property**: The same user ID always routes to the same shard. This is essential for consistency.

**When nodes change**:
- Add a 4th shard: Only ~25% of keys need to move (not 100% like naive sharding)
- Remove a shard: Keys redistribute evenly to remaining shards

This is what makes consistent hashing powerful.

## Step 2: Create a Distributed Pool

```lisp
(defvar *shard-pool*
  (pooler:make-distributed-pool
    :name "Database Shards"
    :node-configs (list
      (list :name "shard-1" :capacity 10 :threshold 2)
      (list :name "shard-2" :capacity 10 :threshold 2)
      (list :name "shard-3" :capacity 10 :threshold 2))
    :item-maker (lambda ()
                  (connect-to-db :host (get-shard-host)
                               :database "user_data"))
    :item-destroyer (lambda (conn)
                      (disconnect-db conn))))
```

**Parameters**:
- `:name` - Identifying name for the distributed pool
- `:node-configs` - List of node configurations (each is a regular pool)
- `:item-maker` - Creates connections (same as basic pools)
- `:item-destroyer` - Cleanup function (same as basic pools)

**Important**: Each node gets its own capacity/threshold. Shards can be different sizes if you want.

## Step 3: Route Requests to Nodes

The key difference from basic pools: **all operations take a key parameter**.

```lisp
;; Fetch a connection for user 123
(let ((connection (pooler:fetch-from-distributed-pool *shard-pool* "user:123")))
  (unwind-protect
    (progn
      ;; This connection is routed to the shard containing user 123
      (query connection "SELECT * FROM users WHERE id = 123"))
    (pooler:return-to-distributed-pool *shard-pool* "user:123" connection)))
```

**Or with the macro**:
```lisp
(pooler:with-pool (connection *shard-pool* "user:123")
  ;; connection is routed to the correct shard
  (query connection "SELECT * FROM users WHERE id = 123"))
```

**Key insight**: The same key always goes to the same shard.

## Step 4: Test Consistency

Let's verify the same key always routes to the same shard:

```lisp
(defun test-consistency (pool key)
  (let ((node1 (pooler:get-node-for-key pool key))
        (node2 (pooler:get-node-for-key pool key))
        (node3 (pooler:get-node-for-key pool key)))
    (format t "Key: ~A~%" key)
    (format t "  First lookup: ~A~%" (pooler::pool-name node1))
    (format t "  Second lookup: ~A~%" (pooler::pool-name node2))
    (format t "  Third lookup: ~A~%" (pooler::pool-name node3))
    (when (and (eq node1 node2) (eq node2 node3))
      (format t "  ✓ Consistent!~%"))))

(test-consistency *shard-pool* "user:123")
;; Output:
;; Key: user:123
;;   First lookup: shard-1
;;   Second lookup: shard-1
;;   Third lookup: shard-1
;;   ✓ Consistent!

(test-consistency *shard-pool* "user:456")
;; Output:
;; Key: user:456
;;   First lookup: shard-2
;;   Second lookup: shard-2
;;   Second lookup: shard-2
;;   ✓ Consistent!
```

## Step 5: Verify Distribution Across Shards

Let's confirm requests distribute evenly across shards:

```lisp
(defun test-distribution (pool key-count)
  (let ((shard-counts (make-hash-table :test 'equal)))
    ;; Generate many keys and see which shard they go to
    (dotimes (i key-count)
      (let* ((key (format nil "user:~A" i))
             (node (pooler:get-node-for-key pool key))
             (shard-name (pooler::pool-name node)))
        (incf (gethash shard-name shard-counts 0))))
    ;; Print results
    (maphash (lambda (shard count)
               (format t "~A: ~A keys (~A%)~%"
                       shard count
                       (round (* 100 count) key-count)))
             shard-counts)))

(test-distribution *shard-pool* 1000)
;; Output:
;; shard-1: 334 keys (33%)
;; shard-2: 333 keys (33%)
;; shard-3: 333 keys (33%)
```

**Why this matters**: Even distribution prevents hotspots where one shard gets all the traffic.

## Step 6: Add a Node Dynamically

You can add a new shard without taking the pool offline:

```lisp
;; Add a 4th shard
(pooler:add-node-to-distributed-pool
  *shard-pool*
  (list :name "shard-4"
        :capacity 10
        :threshold 2))

;; Old keys still route correctly, some new keys go to shard-4
(test-distribution *shard-pool* 1000)
;; Output:
;; shard-1: 244 keys (24%)
;; shard-2: 255 keys (25%)
;; shard-3: 250 keys (25%)
;; shard-4: 251 keys (25%)
;; (About 25% each as expected)
```

## Step 7: Remove a Node

You can remove a shard (though items in it are orphaned):

```lisp
;; Remove shard-2
(pooler:remove-node-from-distributed-pool *shard-pool* "shard-2")

;; Remaining keys redistribute
(test-distribution *shard-pool* 1000)
;; Output:
;; shard-1: 333 keys (33%)
;; shard-3: 333 keys (33%)
;; shard-4: 334 keys (33%)
```

**Note**: Items stored in shard-2 are not automatically migrated. For production, implement manual data migration before removing a shard.

## Step 8: Use Distributed Pools for Multi-Database Sharding

Here's a realistic example with 3 PostgreSQL servers:

```lisp
(defvar *users-pool*
  (pooler:make-distributed-pool
    :name "Users Database (Sharded)"
    :node-configs (list
      (list :name "db-east-1"
            :capacity 15
            :item-maker (lambda () (connect :host "db-east-1.internal" :db "users")))
      (list :name "db-east-2"
            :capacity 15
            :item-maker (lambda () (connect :host "db-east-2.internal" :db "users")))
      (list :name "db-west-1"
            :capacity 15
            :item-maker (lambda () (connect :host "db-west-1.internal" :db "users"))))
    :item-destroyer (lambda (conn) (disconnect conn))))

;; Get a user - always goes to the same shard
(pooler:with-pool (conn *users-pool* "user:alice@example.com")
  (let ((user (query conn "SELECT * FROM users WHERE email = ?"
                     "alice@example.com")))
    (print user)))

;; Update the user - same shard as the fetch
(pooler:with-pool (conn *users-pool* "user:alice@example.com")
  (query conn "UPDATE users SET last_login = NOW() WHERE email = ?"
         "alice@example.com"))
```

## Step 9: Configure Replicas for Better Distribution

By default, each node gets 10 virtual positions on the hash ring. You can adjust this:

```lisp
;; Fewer replicas (less even distribution, smaller hash ring)
(pooler:make-distributed-pool
  :name "Pool"
  :node-configs (list ... )
  :replicas 5)

;; More replicas (very even distribution, larger hash ring)
(pooler:make-distributed-pool
  :name "Pool"
  :node-configs (list ... )
  :replicas 20)
```

**Trade-off**:
- Fewer replicas: Smaller hash ring, slightly less even distribution
- More replicas: Larger hash ring, very even distribution

For most applications, the default of 10 is fine.

## Step 10: Handle Errors with Distributed Pools

```lisp
(handler-case
  (pooler:with-pool (connection *shard-pool* "user:123")
    (query connection "SELECT * FROM users WHERE id = 123"))
  (pooler::pool-error (e)
    ;; Could not get connection to the appropriate shard
    (format t "Shard unavailable: ~A~%" e))
  (error (e)
    ;; Other errors
    (format t "Error: ~A~%" e)))
```

**Common errors**:
- `pool-error`: Connection creation failed for the target shard
- Database errors: Target shard unreachable

## Common Patterns

### Transactions Across Shards

If you need to access multiple shards, explicitly fetch multiple connections:

```lisp
;; Don't do this (wrong!):
(pooler:with-pool (conn1 *shard-pool* "user:123")
  (query conn1 "UPDATE users SET age = 30 WHERE id = 123")
  (pooler:with-pool (conn2 *shard-pool* "user:456")
    (query conn2 "UPDATE users SET referred_by = 123 WHERE id = 456")))
;; Both updates are on different shards, no transaction

;; Better:
(let ((conn1 (pooler:fetch-from-distributed-pool *shard-pool* "user:123"))
      (conn2 (pooler:fetch-from-distributed-pool *shard-pool* "user:456")))
  (unwind-protect
    (progn
      (query conn1 "UPDATE users SET age = 30 WHERE id = 123")
      (query conn2 "UPDATE users SET referred_by = 123 WHERE id = 456")
      ;; Note: These aren't in a distributed transaction!
      ;; Distributed transactions are complex - reconsider if you need them
      )
    (pooler:return-to-distributed-pool *shard-pool* "user:123" conn1)
    (pooler:return-to-distributed-pool *shard-pool* "user:456" conn2)))
```

### Resharding (Adding/Removing Shards)

When adding new shards:
1. Create the new shard with data from an existing shard
2. Verify it's working
3. Add it to the distributed pool with `add-node-to-distributed-pool`
4. Monitor traffic distribution
5. Gradually remove old data from the source shard

When removing shards:
1. Migrate data to remaining shards
2. Verify migration complete
3. Remove from pool with `remove-node-from-distributed-pool`
4. Take down the server

Pooler handles the routing part; you handle the data migration.

## What's Next?

- [Architecture Guide](../architecture.md) - How distributed hashing works
- [API Reference](../reference/api.md) - Complete distributed pool functions
- [How-to: Scale to Multiple Nodes](../how-to/distribute-pool.md) - Production scaling guide
