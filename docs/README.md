# Pooler: Thread-Safe Resource Pooling for Common Lisp

Pooler is a fast, thread-safe library for managing pools of expensive-to-create objects in Common Lisp. Whether you're connecting to databases, managing network sockets, or reusing any costly resource, Pooler handles the lifecycle management automatically.

## Quick Links

- **[Quickstart](quickstart.md)** - Get running in 5 minutes
- **[Core Pooling Tutorial](tutorials/core-pooling.md)** - Learn basic pool operations
- **[Distributed Pooling Tutorial](tutorials/distributed-pooling.md)** - Scale across multiple nodes
- **[API Reference](reference/api.md)** - Complete function documentation
- **[Architecture Guide](architecture.md)** - Understand the design

## The Problem Pooler Solves

Creating and destroying expensive resources repeatedly wastes CPU, memory, and time. A database connection might take 100ms to establish; creating a new one for each request means your application spends more time connecting than querying.

**Pooler's solution**: Keep a pre-created set of resources ("pools") ready to use. When you need one, grab it from the pool. When done, return it. Pooler handles creation, destruction, timeout, and thread safety automatically.

## Installation

Add pooler to your ASDF system definition:

```lisp
(:depends-on (:pooler))
```

Then load it:

```lisp
(ql:quickload :pooler)
```

**Supported on**:
- SBCL (primary)
- Allegro CL
- Other Common Lisp implementations via bordeaux-threads

## Your First Pool (30 seconds)

```lisp
;; Create a pool for numbers (just for demonstration)
(defvar *pool* (pooler:make-pool :name "Demo"
                                :item-maker (lambda () (random 1000))
                                :item-destroyer (lambda (x) nil)))

;; Fetch an item from the pool
(defvar item (pooler:fetch-from *pool*))

;; Use it
(print item)

;; Return it when done
(pooler:return-to *pool* item)
```

In real use, your `item-maker` would create database connections, and `item-destroyer` would close them.

## Two Ways to Use Pooler

### 1. Basic Pooling (Single Pool)

Use `make-pool` when you have one set of resources to manage:

```lisp
(defvar *db-pool*
  (pooler:make-pool
    :name "Database Connections"
    :capacity 10                           ; Max 10 connections
    :threshold 2                           ; Keep 2 ready
    :timeout 300                           ; Reconnect if unused 5 minutes
    :item-maker (lambda () (connect-to-db))
    :item-destroyer (lambda (conn) (close-connection conn))))

;; Use the macro for automatic cleanup
(pooler:with-pool (connection *db-pool*)
  (query connection "SELECT * FROM users"))
```

### 2. Distributed Pooling (Multiple Nodes)

Use `make-distributed-pool` when you want to scale across multiple backend nodes:

```lisp
(defvar *shard-pool*
  (pooler:make-distributed-pool
    :name "Database Shards"
    :node-configs '((:name "shard-1")
                    (:name "shard-2")
                    (:name "shard-3"))
    :item-maker (lambda () (connect-to-db))
    :item-destroyer (lambda (conn) (close-connection conn))))

;; Routes requests consistently to the same shard
(let ((connection (pooler:fetch-from-distributed-pool *shard-pool* "user:123")))
  (query connection "SELECT * FROM user_data WHERE id = 123")
  (pooler:return-to-distributed-pool *shard-pool* "user:123" connection))
```

## Key Concepts

- **Pool**: A container managing multiple items with automatic creation and cleanup
- **Item**: An expensive-to-create resource (database connection, socket, etc.)
- **Capacity**: Maximum items the pool can hold (prevents resource exhaustion)
- **Threshold**: Minimum items to keep ready (prevents creation delays)
- **Timeout**: Automatically refresh pool if unused too long (handles stale connections)

Learn more in [Architecture Guide](architecture.md).

## Common Tasks

- [Create a database connection pool](how-to/setup-database-pool.md)
- [Monitor pool metrics](how-to/monitor-pool.md)
- [Handle pool errors](how-to/handle-errors.md)
- [Scale to multiple nodes](how-to/distribute-pool.md)

## Features

✓ **Automatic lifecycle management** - Creation and cleanup handled by pool
✓ **Thread-safe** - All operations protected by locks
✓ **Distributed** - Route requests across multiple nodes with consistent hashing
✓ **Configurable** - Capacity, timeout, creation/destruction functions
✓ **Metrics** - Track pool utilization and health
✓ **Portable** - Works on SBCL, Allegro CL, and other implementations

## Getting Help

- **Questions**: Check [Architecture Guide](architecture.md) for design decisions
- **Examples**: See [Tutorials](tutorials/) for working examples
- **API Details**: Consult [API Reference](reference/api.md)
- **Issues**: Report bugs on GitHub

## License

MIT License - See LICENSE file in repository
