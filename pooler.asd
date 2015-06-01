;;;; pooler.asd

(asdf:defsystem #:pooler
  :description "Generic thread-safe pooling facility for your library."
  :author "Abhijit Rao"
  :license "MIT"
  :long-description "A Trivial, Fast & Thread-Safe Pooling Library for Common Lisp.

We need pools for items which have heavy cost of creation and which we can reuse.
A typical use case is connection pools.

Pool item creation (as required) is automatic on fetch-from pool. Pool-item's are
created and destroyed using user supplied funcitons. The pool has a idle timeout
after which all the existing pool-item's are destroyed and new ones created
(pool-init). The pool has a threshold number of items which it tries to maintain."
  :serial t
  :depends-on (#-sbcl :bordeaux-threads #+sbcl :sb-concurrency)
  :components ((:file "package")
	       (:file "utils")
               (:file "pooler")))

