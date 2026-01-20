Pooler
======

A Trivial, Fast, Thread-Safe Pooling Library for Common Lisp.

We need pools for items which have heavy cost of creation and which we can reuse.
A typical use case is connection pools.

Pool item creation (as required) is automatic on fetch-from pool. Pool-item's are created and destroyed using user supplied funcitons. The pool has a idle timeout after which all the existing pool-item's are destroyed and new ones created (pool-init). The pool has a threshold number of items which it tries to maintain.

Licence : MIT


API
---

*Structure* **POOL**
```
NAME             : Is a text string identifying the POOL
QUEUE            : A queue to store the POOL-ITEMs
POOL-LOCK        : A lock we hold when we want to update the POOL
ITEM-MAKER       : A function which returns a POOL-ITEM.
ITEM-DESTROYER   : A function which sanely destroys a POOL-ITEM
CAPACITY         : The max number of POOL-ITEMs to store in the POOL
THRESHOLD        : The min number of POOL-ITEMs we should ideally keep in the POOL.
TIMEOUT          : The number of seconds of idleness after which the POOL will be re-init.
LAST-ACCESS      : The last access time for the POOL.
CURRENT-SIZE     : The current number of POOL-ITEMs in the POOL
TOTAL-USES       : Total number of times the POOL-ITEMs have been taken out of the POOL
TOTAL-CREATED    : Total number of new POOL-ITEMs created and added to the POOL
TOTAL-POOL-INITS : How many times the POOL was 'INIT'.
```

---

**make-pool** &key *name* *max-capacity* *min-threshold* *pool-item-maker* *pool-item-destroyer*

Makes and returns a new POOL.

---

**grow-pool** *pool* &optional *grow-by*

Creates and adds POOL-ITEMs to the *pool*. In case *grow-by* is not provided then it takes ( *threshold* *pool* ) as the value

---

**fetch-from** *pool* &key (*tries 2*)

Fetches a POOL-ITEM from the POOL with automatic retry. Will try *tries* number of times to fetch POOL-ITEM from POOL. If the pool is empty, it automatically grows by the threshold amount. Checks for timeout and reinitializes the pool if it has been idle too long.

---

**return-to** *pool* *pool-item*

Returns a POOL-ITEM to the POOL. In case the pool is at CAPACITY the POOL-ITEM will be sanely destroyed using the given function

---

**pool-init** *pool*

Sanely destroys all the POOL-ITEMS and then re-creates THRESHOLD number of POOL-ITEMS.

---

**with-pool** *pool-item* *pool* &body body

Executes the body where *pool-item* is fetched from the *pool* and available. Sanely returns *pool-item* to the *pool* on finish of body.

---

## Distributed Pool API

A distributed pool enables horizontal scaling by distributing items across multiple underlying pool instances (nodes) using consistent hashing. This allows for:
- **Sharding**: Keys map to specific nodes deterministically
- **Session Affinity**: Same key always routes to same node
- **Dynamic Scaling**: Add or remove nodes at runtime
- **Minimal Redistribution**: Only ~1/N keys reroute when adding Nth node

*Structure* **DISTRIBUTED-POOL**
```
NAME         : Text string identifying the distributed pool
NODES        : List of underlying POOL instances (one per node)
HASH-RING    : Sorted array of virtual nodes for consistent hashing
REPLICAS     : Number of virtual nodes per physical node (default: 10)
LOCK         : Lock for protecting hash ring access
```

---

**make-distributed-pool** &key *name* *node-configs* *default-capacity* *default-threshold* *default-timeout* *item-maker* *item-destroyer* *replicas*

Creates a new distributed pool with multiple nodes.

*Parameters*:
- **node-configs**: List of plists, each specifying a node's configuration
  - Each node config must have `:name`
  - Each node config may have `:capacity`, `:threshold`, `:timeout`, `:item-maker`, `:item-destroyer`
- **default-capacity**: Default max items per node (default: 40)
- **default-threshold**: Default min idle items per node (default: 2)
- **default-timeout**: Default idle timeout per node in seconds (default: 300)
- **item-maker**: Global item creation function (used if node config doesn't specify one)
- **item-destroyer**: Global item destruction function (used if node config doesn't specify one)
- **replicas**: Number of virtual nodes per physical node for better distribution (default: 10)

*Example*:
```lisp
(defvar *dist-pool*
  (make-distributed-pool
    :name "DB Connection Pool"
    :node-configs '((:name "node1")
                    (:name "node2")
                    (:name "node3"))
    :default-capacity 20
    :default-threshold 5
    :item-maker #'make-db-connection
    :item-destroyer #'close-db-connection))
```

---

**fetch-from-distributed-pool** *dist-pool* *key* &key (*tries 2*)

Fetches an item from the distributed pool based on a key. The key determines which node to fetch from using consistent hashing.

*Parameters*:
- **dist-pool**: The distributed pool
- **key**: String key for routing (e.g., user ID, session ID, resource name)
- **tries**: Number of retry attempts (default: 2)

*Returns*: A pool item from the node responsible for the key

**Important**: Use the same key for both fetch and return operations.

*Example*:
```lisp
;; Fetch connection for user alice
(defvar *conn* (fetch-from-distributed-pool *dist-pool* "user:alice"))
```

---

**return-to-distributed-pool** *dist-pool* *key* *item*

Returns an item to the distributed pool using the same key that was used to fetch it.

*Parameters*:
- **dist-pool**: The distributed pool
- **key**: Same key used for fetching (MUST match fetch key)
- **item**: The item to return

**Critical**: Always use the same key for fetch and return. Using different keys will corrupt pool state.

*Example*:
```lisp
(let ((key "user:alice"))
  (let ((conn (fetch-from-distributed-pool *dist-pool* key)))
    (unwind-protect
      (query-database conn)
      (return-to-distributed-pool *dist-pool* key conn))))
```

---

**add-node-to-distributed-pool** *dist-pool* *node-config*

Dynamically adds a new node to a running distributed pool.

*Parameters*:
- **dist-pool**: The distributed pool
- **node-config**: Plist with node configuration (must include `:name` and `:item-maker` or use global item-maker)

*Effects*:
- Approximately 1/N keys will reroute to the new node (where N = total nodes after addition)
- Other keys remain on their original nodes
- Hash ring is rebuilt and re-sorted

*Example*:
```lisp
(add-node-to-distributed-pool
  *dist-pool*
  '(:name "node4"
    :capacity 30
    :threshold 10
    :item-maker #'make-db-connection))
```

---

**remove-node-from-distributed-pool** *dist-pool* *node-name*

Removes a node from the distributed pool.

*Parameters*:
- **dist-pool**: The distributed pool
- **node-name**: Name of the node to remove (string)

*Effects*:
- Keys that mapped to the removed node will reroute to other nodes
- Keys on other nodes remain unchanged

**Important Limitation**: Items in the removed node's pool are NOT automatically destroyed or migrated. They are orphaned until manual cleanup.

*Recommended Practice*: Manually drain the node before removal:
```lisp
;; Wait for node to drain
(let ((node (find "node2" (distributed-pool-nodes *dist-pool*)
                  :key #'pool-name :test #'string=)))
  (loop while (> (pool-current-size node) 0)
        do (sleep 1))
  ;; Now remove
  (remove-node-from-distributed-pool *dist-pool* "node2"))
```

---

**get-node-for-key** *dist-pool* *key*

Advanced API: Returns the underlying pool node responsible for a given key.

*Returns*: The POOL struct for the node that handles this key

*Use Cases*:
- Debugging key distribution
- Monitoring node utilization
- Pre-warming specific nodes
- Custom routing logic

*Example*:
```lisp
;; Check which node handles a key
(let ((node (get-node-for-key *dist-pool* "user:alice")))
  (format t "Key routes to: ~A (size: ~A)~%"
          (pool-name node)
          (pool-current-size node)))
```

---

## Distributed Pool Use Cases

### Database Connection Sharding
```lisp
(defvar *shard-pool*
  (make-distributed-pool
    :name "DB Shards"
    :node-configs '((:name "shard-1")
                    (:name "shard-2")
                    (:name "shard-3"))
    :item-maker #'create-db-connection))

;; User data always goes to same shard
(let ((conn (fetch-from-distributed-pool *shard-pool* user-id)))
  (unwind-protect
    (query-user-data conn user-id)
    (return-to-distributed-pool *shard-pool* user-id conn)))
```

### Session Management with Affinity
```lisp
(defvar *session-pool*
  (make-distributed-pool
    :node-configs '((:name "backend-1")
                    (:name "backend-2"))
    :item-maker #'create-session-handler))

;; Sessions stick to same backend
(let ((handler (fetch-from-distributed-pool *session-pool* session-id)))
  (unwind-protect
    (process-session handler session-id)
    (return-to-distributed-pool *session-pool* session-id handler)))
```

### Dynamic Scaling
```lisp
;; Scale up during high load
(when (> current-load high-threshold)
  (add-node-to-distributed-pool
    *dist-pool*
    '(:name "overflow-node"
      :capacity 50
      :item-maker #'make-worker)))

;; Scale down during low load
(when (< current-load low-threshold)
  ;; Drain first, then remove
  (remove-node-from-distributed-pool *dist-pool* "overflow-node"))
```

---

Examples
--------

	POOLER> (defvar *x* nil)
	*X*
	POOLER> (setf *x* (make-pool :name "Test Pool"))
	#<POOL Test Pool Max:40 Current:0 >
	POOLER> (fetch-from *x*)
	1
	POOLER> *x*
	#<POOL Test Pool Max:40 Current:1 >
	POOLER> (return-to *x* **)
	2
	POOLER> (with-pool (pool-item *x*) (print pool-item))
	1
	1
	POOLER> *x*
	#<POOL Test Pool Max:40 Current:2 >

Another

    CL-USER> (pooler:make-pool :item-maker #'(lambda () (clsql:connect '("127.0.0.1" "quasidb" "quasi" "*****")
                                                                        :database-type :mysql
                                                                        :if-exists :new))
                               :item-destroyer #'(lambda (item) (clsql:disconnect :database item)))
    #S(POOLER::POOL
      :NAME "Default Pool"
      :QUEUE #S(SB-CONCURRENCY:QUEUE
      :HEAD (SB-CONCURRENCY::.DUMMY.)
      :TAIL (SB-CONCURRENCY::.DUMMY.)
      :NAME NIL)
      :LOCK #<SB-THREAD:MUTEX "Pool Lock" (free)>
      :ITEM-MAKER #<FUNCTION (LAMBDA #) {1005C9BFAB}>
      :ITEM-DESTROYER #<FUNCTION (LAMBDA #) {1005CCAAAB}>
      :CAPACITY 40
      :THRESHOLD 2
      :TIMEOUT 300
      :LAST-ACCESS 0
      :CURRENT-SIZE 0
      :TOTAL-USES 0
      :TOTAL-CREATED 0
      :TOTAL-POOL-INITS 0)
    CL-USER> (defvar *mysql-pool* *)
    CL-USER> (pooler:fetch-from *mysql-pool*)
    #<CLSQL-MYSQL:MYSQL-DATABASE 127.0.0.1/quasidb/quasi OPEN {1007571373}>
    CL-USER> (pooler:return-to *mysql-pool* *)
    2
    CL-USER> (pooler:with-pool (db *mysql-pool*) (clsql:query "show tables;" :database db))
    (("LOGIN_DATA"))
    ("Tables_in_quasidb")


Author
------
Abhijit Rao a.k.a quasi

quasi@quasilabs.in
