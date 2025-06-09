;;;; package.lisp

(defpackage #:pooler
  (:use #:cl)
  (:export #:make-pool
	   #:grow-pool
	   #:pool-init
	   #:fetch-from
	   #:return-to
	   #:with-pool
	   #:make-distributed-pool
	   #:get-node-for-key
	   #:fetch-from-distributed-pool
	   #:return-to-distributed-pool
	   #:add-node-to-distributed-pool
	   #:remove-node-from-distributed-pool))

