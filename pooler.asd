;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;
;;;  Pooler: A generic thread-safe pooling library
;;;  (c) Abhijit 'quasi' Rao
;;;

(asdf:defsystem #:pooler
  :version "1.0.0"
  :description "Generic thread-safe pooling facility for your library."
  :author "quasi <quasi@quasilabs.in>"
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

(asdf:defsystem #:pooler/tests
  :description "Test system for Pooler."
  :author "Jules AI" ; You can change this
  :license "MIT"
  :depends-on (#:pooler)
  :components ((:module "tests"
                :pathname "tests/"
                :components
                ((:file "test-pooler"))))
  :perform (asdf:test-op (op c) (declare (ignore op c))
                         (asdf:symbol-call '#:pooler-tests '#:run-all-tests)))
