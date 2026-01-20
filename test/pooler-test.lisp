;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; pooler-test.lisp

(defpackage #:pooler-test
  (:use #:cl #:fiveam #:pooler)
  (:export #:run-tests))

(in-package #:pooler-test)

(def-suite pooler-test
  :description "Test suite for pooler library")

(in-suite pooler-test)

;;; Helper functions for testing

(defun make-counter-maker ()
  "Returns a function that creates incrementing numbers"
  (let ((counter 0))
    (lambda ()
      (incf counter))))

(defun make-list-tracker ()
  "Returns maker and destroyer functions that track created/destroyed items in a list"
  (let ((created nil)
        (destroyed nil)
        (counter 0))
    (values
     (lambda () (let ((item (incf counter)))
                  (push item created)
                  item))
     (lambda (item) (push item destroyed))
     (lambda () created)
     (lambda () destroyed))))

;;; Basic Pool Creation Tests

(test make-pool-basic
  "Test basic pool creation with default values"
  (let ((pool (make-pool :name "Test Pool")))
    (is (not (null pool)))
    (is (string= "Test Pool" (pooler::pool-name pool)))
    (is (= 40 (pooler::pool-capacity pool)))
    (is (= 2 (pooler::pool-threshold pool)))
    (is (= 300 (pooler::pool-timeout pool)))
    (is (= 0 (pooler::pool-current-size pool)))))

(test make-pool-custom-values
  "Test pool creation with custom values"
  (let ((pool (make-pool :name "Custom Pool"
                         :capacity 10
                         :threshold 5
                         :timeout 600)))
    (is (= 10 (pooler::pool-capacity pool)))
    (is (= 5 (pooler::pool-threshold pool)))
    (is (= 600 (pooler::pool-timeout pool)))))

(test make-pool-custom-functions
  "Test pool creation with custom item-maker and item-destroyer"
  (let* ((created nil)
         (destroyed nil)
         (pool (make-pool :name "Custom Functions Pool"
                          :item-maker (lambda () (push 'created created) 'test-item)
                          :item-destroyer (lambda (item) (push item destroyed)))))
    (is (functionp (pooler::pool-item-maker pool)))
    (is (functionp (pooler::pool-item-destroyer pool)))))

;;; Pool Growth Tests

(test grow-pool-default
  "Test pool growth with default threshold"
  (let ((pool (make-pool :name "Growth Test"
                         :threshold 3
                         :item-maker (lambda () (random 1000)))))
    (grow-pool pool)
    (is (= 3 (pooler::pool-current-size pool)))
    (is (= 3 (pooler::pool-total-created pool)))))

(test grow-pool-custom-amount
  "Test pool growth with custom amount"
  (let ((pool (make-pool :name "Growth Test"
                         :item-maker (lambda () (random 1000)))))
    (grow-pool pool 5)
    (is (= 5 (pooler::pool-current-size pool)))
    (is (= 5 (pooler::pool-total-created pool)))))

(test grow-pool-respects-capacity
  "Test that pool growth respects maximum capacity"
  (let ((pool (make-pool :name "Capacity Test"
                         :capacity 5
                         :item-maker (lambda () (random 1000)))))
    (grow-pool pool 10)
    (is (<= (pooler::pool-current-size pool) 5))
    (is (<= (pooler::pool-total-created pool) 5))))

;;; Fetch and Return Tests

(test fetch-from-empty-pool
  "Test fetching from an empty pool auto-grows it"
  (let ((pool (make-pool :name "Fetch Test"
                         :threshold 2
                         :item-maker (lambda () (random 1000)))))
    (let ((item (fetch-from pool)))
      (is (not (null item)))
      (is (= 1 item)))))

(test fetch-from-decrements-size
  "Test that fetching decrements pool size"
  (let ((pool (make-pool :name "Fetch Test"
                         :item-maker (lambda () (random 1000)))))
    (grow-pool pool 3)
    (is (= 3 (pooler::pool-current-size pool)))
    (fetch-from pool)
    (is (= 2 (pooler::pool-current-size pool)))))

(test fetch-from-updates-stats
  "Test that fetching updates total-uses counter"
  (let ((pool (make-pool :name "Stats Test"
                         :item-maker (lambda () (random 1000)))))
    (is (= 0 (pooler::pool-total-uses pool)))
    (fetch-from pool)
    (is (= 1 (pooler::pool-total-uses pool)))
    (fetch-from pool)
    (is (= 2 (pooler::pool-total-uses pool)))))

(test return-to-increments-size
  "Test that returning an item increments pool size"
  (let ((pool (make-pool :name "Return Test"
                         :threshold 2
                         :item-maker (lambda () (random 1000)))))
    (let ((item (fetch-from pool)))
      ;; fetch-from grows by threshold (2), fetches 1, leaving 1
      (return-to pool item)
      ;; Now we should have 2 items (1 left + 1 returned)
      (is (= 2 (pooler::pool-current-size pool))))))

(test return-to-respects-capacity
  "Test that returning to a full pool destroys the item"
  (multiple-value-bind (maker destroyer get-created get-destroyed)
      (make-list-tracker)
    (let ((pool (make-pool :name "Capacity Test"
                           :capacity 2
                           :item-maker maker
                           :item-destroyer destroyer)))
      ;; Fill the pool to capacity
      (grow-pool pool 2)
      (is (= 2 (pooler::pool-current-size pool)))
      ;; Create a new item outside the pool
      (let ((extra-item (funcall maker)))
        ;; Try to return it - should be destroyed since pool is at capacity
        (return-to pool extra-item)
        (is (= 2 (pooler::pool-current-size pool)))
        (is (member extra-item (funcall get-destroyed)))))))

(test fetch-and-return-cycle
  "Test multiple fetch-return cycles"
  (let ((pool (make-pool :name "Cycle Test"
                         :item-maker (lambda () (random 1000)))))
    (dotimes (i 5)
      (let ((item (fetch-from pool)))
        (is (not (null item)))
        (return-to pool item)))
    (is (= 5 (pooler::pool-total-uses pool)))))

;;; Pool Init Tests

(test pool-init-destroys-all-items
  "Test that pool-init destroys all existing items"
  (multiple-value-bind (maker destroyer get-created get-destroyed)
      (make-list-tracker)
    (let ((pool (make-pool :name "Init Test"
                           :threshold 3
                           :item-maker maker
                           :item-destroyer destroyer)))
      (grow-pool pool 5)
      (let ((created-before (length (funcall get-created))))
        (pool-init pool)
        ;; All items should be destroyed
        (is (= created-before (length (funcall get-destroyed))))))))

(test pool-init-resets-counters
  "Test that pool-init resets pool counters"
  (let ((pool (make-pool :name "Init Test"
                         :threshold 2
                         :item-maker (lambda () (random 1000)))))
    (grow-pool pool 5)
    (dotimes (i 3) (fetch-from pool))
    (handler-bind ((warning #'muffle-warning))
      (pool-init pool))
    (is (= 0 (pooler::pool-total-uses pool)))
    ;; pool-init calls grow-pool at the end, so total-created = threshold
    (is (= 2 (pooler::pool-total-created pool)))
    (is (= 1 (pooler::pool-total-pool-inits pool)))))

(test pool-init-regrows-to-threshold
  "Test that pool-init regrows pool to threshold"
  (let ((pool (make-pool :name "Init Test"
                         :threshold 4
                         :item-maker (lambda () (random 1000)))))
    (pool-init pool)
    (is (= 4 (pooler::pool-current-size pool)))))

;;; Timeout Tests

(test pool-timeout-triggers-reinit
  "Test that pool timeout triggers reinitialization"
  (let ((pool (make-pool :name "Timeout Test"
                         :threshold 2
                         :timeout 1  ; 1 second timeout
                         :item-maker (lambda () (random 1000)))))
    (grow-pool pool 2)
    (let ((old-total-inits (pooler::pool-total-pool-inits pool)))
      ;; Wait for timeout
      (sleep 2)
      ;; Next fetch should trigger pool-init
      (handler-bind ((warning #'muffle-warning))
        (fetch-from pool))
      (is (= (1+ old-total-inits) (pooler::pool-total-pool-inits pool))))))

;;; with-pool Macro Tests

(test with-pool-basic
  "Test basic with-pool macro usage"
  (let ((pool (make-pool :name "Macro Test"
                         :threshold 2
                         :item-maker (lambda () (random 1000)))))
    (let ((result (with-pool (item pool)
                    (is (not (null item)))
                    item)))
      (is (not (null result)))
      ;; fetch-from grows by threshold (2), fetches 1, then returns 1
      ;; So we should have 2 items total
      (is (= 2 (pooler::pool-current-size pool))))))

(test with-pool-returns-on-error
  "Test that with-pool returns item even on error"
  (let ((pool (make-pool :name "Error Test"
                         :threshold 2
                         :item-maker (lambda () (random 1000)))))
    (ignore-errors
      (with-pool (item pool)
        (error "Test error")))
    ;; Item should still be returned to pool
    ;; fetch-from grows by threshold (2), fetches 1, then returns 1
    (is (= 2 (pooler::pool-current-size pool)))))

(test with-pool-returns-on-normal-exit
  "Test that with-pool returns item on normal exit"
  (let ((pool (make-pool :name "Normal Exit Test"
                         :threshold 2
                         :item-maker (lambda () (random 1000)))))
    (with-pool (item pool)
      (is (not (null item))))
    ;; fetch-from grows by threshold (2), fetches 1, then returns 1
    (is (= 2 (pooler::pool-current-size pool)))))

(test with-pool-nested
  "Test nested with-pool calls"
  (let ((pool (make-pool :name "Nested Test"
                         :threshold 3
                         :item-maker (lambda () (random 1000)))))
    (with-pool (item1 pool)
      (with-pool (item2 pool)
        (is (not (equal item1 item2)))))
    ;; First fetch grows by threshold (3), fetches 1, leaving 2
    ;; Second fetch takes 1, leaving 1
    ;; Both return, so final size is 3
    (is (= 3 (pooler::pool-current-size pool)))))

;;; Error Condition Tests

(test pool-item-creation-error
  "Test that errors in item-maker are properly signaled"
  (let ((pool (make-pool :name "Error Test"
                         :item-maker (lambda () (error "Creation failed")))))
    (signals pooler::pool-item-creation-error
      (grow-pool pool))))

;;; Thread Safety Tests (if threading is available)

#+sbcl
(test concurrent-fetch-and-return
  "Test concurrent fetch and return operations"
  (let ((pool (make-pool :name "Concurrent Test"
                         :threshold 10
                         :capacity 20
                         :item-maker (lambda () (random 1000))))
        (threads nil))
    (grow-pool pool 10)
    ;; Create 5 threads that each fetch and return 10 times
    (dotimes (i 5)
      (push (sb-thread:make-thread
             (lambda ()
               (dotimes (j 10)
                 (let ((item (fetch-from pool)))
                   (sleep 0.001) ; Small delay
                   (when item
                     (return-to pool item))))))
            threads))
    ;; Wait for all threads to complete
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    ;; Pool should be consistent
    (is (<= (pooler::pool-current-size pool) 20))
    (is (= 50 (pooler::pool-total-uses pool)))))

;;; Integration Tests

(test full-lifecycle-test
  "Test complete pool lifecycle"
  (multiple-value-bind (maker destroyer get-created get-destroyed)
      (make-list-tracker)
    (let ((pool (make-pool :name "Lifecycle Test"
                           :threshold 3
                           :capacity 10
                           :item-maker maker
                           :item-destroyer destroyer)))
      ;; Initial growth
      (grow-pool pool)
      (is (= 3 (pooler::pool-current-size pool)))

      ;; Fetch and return
      (let ((item (fetch-from pool)))
        (is (not (null item)))
        (return-to pool item))

      ;; Verify stats
      (is (= 1 (pooler::pool-total-uses pool)))

      ;; Pool init
      (handler-bind ((warning #'muffle-warning))
        (pool-init pool))
      (is (= 3 (pooler::pool-current-size pool)))
      (is (= 1 (pooler::pool-total-pool-inits pool)))

      ;; Verify cleanup happened
      (is (>= (length (funcall get-destroyed)) 3)))))

(test realistic-connection-pool-simulation
  "Test simulating a realistic connection pool scenario"
  (let* ((connection-counter 0)
         (pool (make-pool :name "Connection Pool"
                          :threshold 5
                          :capacity 20
                          :timeout 300
                          :item-maker (lambda ()
                                        (incf connection-counter)
                                        (format nil "connection-~A" connection-counter))
                          :item-destroyer (lambda (conn)
                                            (declare (ignore conn))
                                            nil))))
    ;; Simulate multiple clients using connections
    (dotimes (i 10)
      (with-pool (conn pool)
        (is (stringp conn))
        (is (search "connection-" conn))))

    ;; Should have created some connections
    (is (> (pooler::pool-total-created pool) 0))
    ;; Should have reused connections
    (is (= 10 (pooler::pool-total-uses pool)))
    ;; Should not have created more than necessary
    (is (<= (pooler::pool-total-created pool) 20))))

;;;; Distributed Pool Tests

(test distributed-pool-creation
  "Test creating distributed pool with multiple nodes"
  (let ((dp (pooler:make-distributed-pool
              :name "TestDistPool"
              :node-configs (list (list :name "node1")
                                  (list :name "node2")
                                  (list :name "node3"))
              :default-capacity 5
              :default-threshold 2
              :item-maker (lambda () (random 1000)))))
    (is (not (null dp)))
    (is (string= "TestDistPool" (pooler::distributed-pool-name dp)))
    (is (= 3 (length (pooler::distributed-pool-nodes dp))))
    (is (= 30 (length (pooler::distributed-pool-hash-ring dp)))) ; 3 nodes × 10 replicas
    ;; Each node should have correct config
    (dolist (node (pooler::distributed-pool-nodes dp))
      (is (= 5 (pooler::pool-capacity node)))
      (is (= 2 (pooler::pool-threshold node))))))

(test distributed-pool-custom-replicas
  "Test creating distributed pool with custom replica count"
  (let ((dp (pooler:make-distributed-pool
              :node-configs (list (list :name "n1") (list :name "n2"))
              :replicas 20
              :item-maker (lambda () (random 1000)))))
    (is (= 40 (length (pooler::distributed-pool-hash-ring dp)))) ; 2 nodes × 20 replicas
    (is (= 20 (pooler::distributed-pool-replicas dp)))))

(test distributed-pool-fetch-and-return
  "Test fetching and returning items using keys"
  (let ((dp (pooler:make-distributed-pool
              :node-configs (list (list :name "node1") (list :name "node2"))
              :default-threshold 1
              :item-maker (lambda () (random 1000)))))
    ;; Fetch with key
    (let ((item (pooler:fetch-from-distributed-pool dp "user:alice")))
      (is (not (null item)))
      ;; Return with same key
      (is (numberp (pooler:return-to-distributed-pool dp "user:alice" item))))))  ; Returns new size

(test distributed-pool-key-consistency
  "Test that same key always maps to same node"
  (let ((dp (pooler:make-distributed-pool
              :node-configs (list (list :name "n1") (list :name "n2") (list :name "n3"))
              :item-maker (lambda () (random 1000)))))
    (let ((node1 (pooler:get-node-for-key dp "user:alice"))
          (node2 (pooler:get-node-for-key dp "user:alice"))
          (node3 (pooler:get-node-for-key dp "user:alice")))
      (is (eq node1 node2))
      (is (eq node2 node3))
      ;; Same key, same node (deterministic)
      (is (string= (pooler::pool-name node1)
                   (pooler::pool-name node2))))))

(test distributed-pool-key-distribution
  "Test that keys distribute across nodes"
  (let* ((dp (pooler:make-distributed-pool
               :node-configs (list (list :name "nodeA") (list :name "nodeB"))
               :replicas 20
               :item-maker (lambda () (random 1000))))
         (node-counts (make-hash-table :test 'equal)))
    ;; Map 50 keys
    (dotimes (i 50)
      (let* ((key (format nil "key-~A" i))
             (node (pooler:get-node-for-key dp key))
             (name (pooler::pool-name node)))
        (incf (gethash name node-counts 0))))
    ;; Both nodes should receive some keys
    (is (> (gethash "nodeA" node-counts 0) 0))
    (is (> (gethash "nodeB" node-counts 0) 0))
    ;; Total should be 50
    (is (= 50 (+ (gethash "nodeA" node-counts 0)
                 (gethash "nodeB" node-counts 0))))))

(test distributed-pool-add-node
  "Test adding node to running distributed pool"
  (let ((dp (pooler:make-distributed-pool
              :node-configs (list (list :name "n1") (list :name "n2"))
              :replicas 5
              :item-maker (lambda () (random 1000)))))
    (is (= 2 (length (pooler::distributed-pool-nodes dp))))
    (is (= 10 (length (pooler::distributed-pool-hash-ring dp))))
    ;; Add node
    (pooler:add-node-to-distributed-pool
      dp
      (list :name "n3" :capacity 10 :item-maker (lambda () (random 1000))))
    (is (= 3 (length (pooler::distributed-pool-nodes dp))))
    (is (= 15 (length (pooler::distributed-pool-hash-ring dp))))
    ;; Node3 should be findable
    (is (find "n3" (pooler::distributed-pool-nodes dp)
              :key #'pooler::pool-name :test #'string=))))

(test distributed-pool-remove-node
  "Test removing node from distributed pool"
  (let ((dp (pooler:make-distributed-pool
              :node-configs (list (list :name "n1") (list :name "n2") (list :name "n3"))
              :item-maker (lambda () (random 1000)))))
    (is (= 3 (length (pooler::distributed-pool-nodes dp))))
    ;; Remove node
    (pooler:remove-node-from-distributed-pool dp "n2")
    (is (= 2 (length (pooler::distributed-pool-nodes dp))))
    ;; Node2 should not be in list
    (is (not (find "n2" (pooler::distributed-pool-nodes dp)
                   :key #'pooler::pool-name :test #'string=)))
    ;; Node2's virtual nodes should not be in hash ring
    (is (not (find "n2" (pooler::distributed-pool-hash-ring dp)
                   :key #'pooler::hash-ring-node-id :test #'string=)))))

(test distributed-pool-hash-ring-sorted
  "Test that hash ring is always sorted"
  (let ((dp (pooler:make-distributed-pool
              :node-configs (list (list :name "n1") (list :name "n2") (list :name "n3"))
              :item-maker (lambda () (random 1000)))))
    (let ((ring (pooler::distributed-pool-hash-ring dp)))
      (loop for i from 0 below (1- (length ring))
            do (is (<= (pooler::hash-ring-node-hash-value (nth i ring))
                       (pooler::hash-ring-node-hash-value (nth (1+ i) ring))))))))

(test distributed-pool-error-empty-ring
  "Test fetching from empty ring signals error"
  (let ((dp (pooler:make-distributed-pool
              :node-configs (list (list :name "n1"))
              :item-maker (lambda () (random 1000)))))
    ;; Remove all nodes
    (pooler:remove-node-from-distributed-pool dp "n1")
    ;; Fetch should error
    (signals pooler::pool-error
      (pooler:fetch-from-distributed-pool dp "key1"))))

(test distributed-pool-error-no-item-maker
  "Test creating pool without item-maker signals error"
  (signals pooler::pool-error
    (pooler:make-distributed-pool
      :name "BrokenPool"
      :node-configs (list (list :name "node1")))))

(test distributed-pool-error-remove-nonexistent
  "Test removing non-existent node signals error"
  (let ((dp (pooler:make-distributed-pool
              :node-configs (list (list :name "n1"))
              :item-maker (lambda () (random 1000)))))
    (signals pooler::pool-error
      (pooler:remove-node-from-distributed-pool dp "nonexistent"))))

(test distributed-pool-error-add-missing-maker
  "Test adding node without item-maker (when no global) signals error"
  (let ((dp (pooler:make-distributed-pool
              :node-configs (list (list :name "n1" :item-maker (lambda () (random 1000)))))))
    (signals pooler::pool-error
      (pooler:add-node-to-distributed-pool dp (list :name "n2")))))

(test distributed-pool-per-node-config
  "Test nodes can have different configurations"
  (let ((dp (pooler:make-distributed-pool
              :node-configs (list (list :name "small" :capacity 5 :threshold 1 :item-maker (lambda () (random 1000)))
                                  (list :name "large" :capacity 20 :threshold 5 :item-maker (lambda () (random 1000)))))))
    (let ((small-node (find "small" (pooler::distributed-pool-nodes dp)
                            :key #'pooler::pool-name :test #'string=))
          (large-node (find "large" (pooler::distributed-pool-nodes dp)
                            :key #'pooler::pool-name :test #'string=)))
      (is (= 5 (pooler::pool-capacity small-node)))
      (is (= 1 (pooler::pool-threshold small-node)))
      (is (= 20 (pooler::pool-capacity large-node)))
      (is (= 5 (pooler::pool-threshold large-node))))))

(test distributed-pool-auto-growth
  "Test distributed pool auto-grows underlying nodes"
  (let ((dp (pooler:make-distributed-pool
              :node-configs (list (list :name "n1"))
              :default-threshold 3
              :item-maker (lambda () (random 1000)))))
    (let ((node (first (pooler::distributed-pool-nodes dp))))
      (is (= 0 (pooler::pool-current-size node))) ; Empty initially
      ;; Fetch should auto-grow
      (let ((item (pooler:fetch-from-distributed-pool dp "key1")))
        (is (not (null item)))
        ;; Node grew by threshold (3), then 1 was fetched
        (is (= 2 (pooler::pool-current-size node))) ; 3 created - 1 fetched
        (is (= 3 (pooler::pool-total-created node)))
        (pooler:return-to-distributed-pool dp "key1" item)))))

(test distributed-pool-fetch-retry
  "Test distributed pool fetch retry behavior when item-maker fails"
  (let ((call-count 0))
    (let ((dp (pooler:make-distributed-pool
                :node-configs (list (list :name "n1"))
                :item-maker (lambda ()
                              (incf call-count)
                              (error "Simulated failure")))))
      ;; Fetch should signal pool-item-creation-error
      (signals pooler::pool-item-creation-error
        (pooler:fetch-from-distributed-pool dp "key1" :tries 3)))))

#+sbcl
(test distributed-pool-concurrent-fetches
  "Test concurrent fetches from distributed pool"
  (let ((dp (pooler:make-distributed-pool
              :node-configs (list (list :name "n1") (list :name "n2"))
              :item-maker (lambda () (random 1000))))
        (results nil)
        (lock (sb-thread:make-mutex)))
    (let ((threads
            (loop for i from 1 to 10
                  collect (sb-thread:make-thread
                            (lambda ()
                              (let* ((key (format nil "key-~A" (random 100)))
                                     (item (pooler:fetch-from-distributed-pool dp key)))
                                (sb-thread:with-mutex (lock)
                                  (push item results))
                                (pooler:return-to-distributed-pool dp key item)))
                            :name (format nil "worker-~A" i)))))
      (mapc #'sb-thread:join-thread threads)
      ;; All threads completed
      (is (= 10 (length results)))
      ;; All items are non-null
      (is (every #'identity results)))))

(test distributed-pool-multiple-keys
  "Test multiple distinct keys route to potentially different nodes"
  (let ((dp (pooler:make-distributed-pool
              :node-configs (list (list :name "n1") (list :name "n2") (list :name "n3"))
              :item-maker (lambda () (random 1000)))))
    (let ((item-a (pooler:fetch-from-distributed-pool dp "user:alice"))
          (item-b (pooler:fetch-from-distributed-pool dp "user:bob"))
          (item-c (pooler:fetch-from-distributed-pool dp "session:xyz")))
      (is (not (null item-a)))
      (is (not (null item-b)))
      (is (not (null item-c)))
      ;; Return all
      (pooler:return-to-distributed-pool dp "user:alice" item-a)
      (pooler:return-to-distributed-pool dp "user:bob" item-b)
      (pooler:return-to-distributed-pool dp "session:xyz" item-c)
      ;; Verify total items created across all nodes
      (let ((total (loop for node in (pooler::distributed-pool-nodes dp)
                         sum (pooler::pool-total-created node))))
        (is (>= total 3)))))) ; At least 3 items created

;;;; Test runner function

(defun run-tests ()
  "Run all pooler tests"
  (run! 'pooler-test))
