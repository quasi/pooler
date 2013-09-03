Pooler
======

A Trivial Fast Thread-Safe Pooling Library for Common Lisp.

We need pools for items which have heavy cost of creation and which we can reuse.
A typical use case is connection pools.


API
---

*Structure* **POOL**
```
NAME : Is a text string identifying the POOL
QUEUE : A queue to store the POOL-ITEMs
POOL-LOCK : A lock we hold when we want to update the POOL
ITEM-MAKER : A function which returns a POOL-ITEM.
ITEM-DESTROYER : A function which sanely destroys a POOL-ITEM
CAPACITY : The max number of POOL-ITEMs to store in the POOL
THRESHOLD : The min number of POOL-ITEMs we should ideally keep in the POOL.
CURRENT-SIZE : The current number of POOL-ITEMs in the POOL
TOTAL-USES : Total number of times the POOL-ITEMs have been taken out of the POOL
TOTAL-CREATED : Total number of new POOL-ITEMs created and added to the POOL
```

---

**make-pool** &key *name* *max-capacity* *min-threshold* *pool-item-maker* *pool-item-destroyer*

Makes and returns a new POOL.

---

**grow-pool** *pool* &optional *grow-by*

Creates and adds POOL-ITEMs to the *pool*. In case *grow-by* is not provided then it takes ( *threshold* *pool* ) as the value

---

**fetch-from** *pool*

Fetches a POOL-ITEM from the POOL.

---

**fetch-from+** *pool* &key (*tries 3*)

Is a wrapper around *fetch-from* and will try *tries* number of times to fetch POOL-ITEM from POOL. In case POOL-ITEM is not returned then it grows the POOL and tries again.

---

**return-to** *pool* *pool-item*

Returns a POOL-ITEM to the POOL. In case the pool is at CAPACITY the POOL-ITEM will be sanely destroyed using the given function

---

**pool-init** *pool*

Sanely destroys all the POOL-ITEMS and then re-creates THRESHOLD number of POOL-ITEMS.

---

**with-pool** *pool-item* *pool* &body body

Executes the body where *pool-item* is fetched from the *pool* and available. Sanely returns *pool-item* to the *pool* on finish of body.


Examples
--------

	POOLER> (defvar *x* nil)
	*X*
	POOLER> (setf *x* (make-pool :name "Test Pool"))
	#<POOL Test Pool Max:4 Current:0 >
	POOLER> (fetch-from+ *x*)
	SAMPLE-ITEM
	POOLER> *x*
	#<POOL Test Pool Max:4 Current:1 >
	POOLER> (return-to *x* **)
	2
	POOLER> (with-pool (pool-item *x*) (print pool-item))
	SAMPLE-ITEM
	SAMPLE-ITEM
	POOLER> *x*
	#<POOL Test Pool Max:4 Current:2 >



Author
------
```
  Abhijit Rao a.k.a quasi
  (reverse "ni.sbalisauq@isauq")
  Licence : MIT
```
