# Quickstart: Your First Pool in 5 Minutes

This guide gets you from zero to a working pool in the shortest time possible.

## Prerequisites

- Common Lisp implementation (SBCL recommended)
- Quicklisp set up
- Basic Lisp familiarity

## Step 1: Load Pooler

```lisp
(ql:quickload :pooler)
```

If this fails, check that Quicklisp is installed and up to date.

## Step 2: Create a Pool

Let's create a simple pool that manages numbers (we'll use a real database next):

```lisp
(defvar *my-pool*
  (pooler:make-pool
    :name "Quick Demo"
    :capacity 5                                    ; Max 5 items
    :threshold 2                                   ; Keep 2 ready
    :item-maker (lambda () (format nil "Item ~A" (random 1000)))
    :item-destroyer (lambda (item) nil)))
```

**What this means**:
- Name: "Quick Demo" (for debugging)
- Capacity: Pool holds at most 5 items
- Threshold: Pool pre-creates 2 items on startup
- Item Maker: Function that creates new items
- Item Destroyer: Function to clean up items (not needed here)

## Step 3: Fetch and Return Items

```lisp
;; Get an item from the pool
(defvar item (pooler:fetch-from *my-pool*))

;; Use it
(print item)
;; Output: "Item 847"

;; Return it when done
(pooler:return-to *my-pool* item)
```

**Expected behavior**:
- First fetch creates items up to threshold
- Returns a string like "Item 847"
- After return, item is available for next fetch

## Step 4: Use the Convenience Macro

The previous way works, but `with-pool` is safer (returns item even if error occurs):

```lisp
(pooler:with-pool (item *my-pool*)
  (print item))
;; Item automatically returned after block exits
```

## Step 5: Check Pool Status

```lisp
;; Current items available
(pooler::pool-current-size *my-pool*)

;; Total items created
(pooler::pool-total-created *my-pool*)

;; Total times items have been fetched
(pooler::pool-total-uses *my-pool*)
```

*Note*: Metrics use `::` because they're internal slots. In real applications, monitor higher-level metrics.

## Real-World Example: Database Connections

Now let's use a real resource. Assuming you have a function `connect-to-db`:

```lisp
(defvar *db-pool*
  (pooler:make-pool
    :name "Database Pool"
    :capacity 10
    :threshold 2
    :timeout 300                           ; Reconnect after 5 minutes
    :item-maker (lambda ()
                  (connect-to-db :host "localhost" :database "mydb"))
    :item-destroyer (lambda (conn)
                      (disconnect-db conn))))

;; Use it
(pooler:with-pool (connection *db-pool*)
  (let ((results (query connection "SELECT * FROM users")))
    (process-results results)))
```

**Key difference from the demo**:
- `item-destroyer` now actually does cleanup (closes connections)
- `timeout` set to 300 seconds to refresh stale connections
- Pool size tuned for your database (10 connections typical for web apps)

## What's Next?

- **Multiple Pools**: See [Tutorial: Multiple Pools](tutorials/core-pooling.md)
- **Distributed Pools**: Scale across nodes with [Distributed Pooling](tutorials/distributed-pooling.md)
- **Monitoring**: Track pool health in [How-to: Monitor Pool](how-to/monitor-pool.md)
- **Errors**: Handle edge cases in [How-to: Handle Errors](how-to/handle-errors.md)

## Common Issues

**"Pool item creation failed"**
- Your `item-maker` threw an error
- Pool retries automatically, but eventually gives up
- Fix: Make sure `item-maker` works before using pool

**"Fetch timeout"**
- Pool couldn't get an item after several tries
- Usually means `item-maker` consistently fails
- Fix: Check your creation function

**"Performance is slow"**
- Pool might be undersized
- Increase `capacity` and `threshold`
- Or switch to distributed pooling

---

That's it! You now have a working pool. Proceed to the full tutorials for more advanced use cases.
