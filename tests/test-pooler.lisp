;;; tests/test-pooler.lisp
(defpackage #:pooler-tests
  (:use #:cl #:pooler)
  (:export #:run-all-tests))

(in-package #:pooler-tests)

(defvar *test-item-counter* 0)
(defvar *destroyed-items* nil)

(defun make-test-item ()
  (incf *test-item-counter*)
  (format nil "item-~A" *test-item-counter*))

(defun destroy-test-item (item)
  (push item *destroyed-items*)
  ;; (format t "~&Destroyed test item: ~A~%" item)
  )

(defun reset-test-globals ()
  (setf *test-item-counter* 0)
  (setf *destroyed-items* nil))

(defun test-basic-distributed-pool ()
  (reset-test-globals)
  (let* ((node-configs (list (list :name "node1")
                             (list :name "node2")
                             (list :name "node3")))
         (dp (pooler:make-distributed-pool :name "TestDistPool"
                                           :node-configs node-configs
                                           :item-maker #'make-test-item
                                           :item-destroyer #'destroy-test-item
                                           :default-capacity 5
                                           :default-threshold 1)))
    (assert dp)
    (assert (string= (pooler::distributed-pool-name dp) "TestDistPool"))
    (assert (= (length (pooler::distributed-pool-nodes dp)) 3))
    (assert (pooler::distributed-pool-hash-ring dp))

    ;; Fetch and return
    (let ((item1 (pooler:fetch-from-distributed-pool dp "keyA")))
      (assert item1)
      ;; With threshold 1, each node should have made one item upon creation.
      ;; The first fetch for a key might get a pre-made item or make a new one if the pool is empty.
      (pooler:return-to-distributed-pool dp "keyA" item1))

    (let ((item2 (pooler:fetch-from-distributed-pool dp "keyB")))
      (assert item2)
      (pooler:return-to-distributed-pool dp "keyB" item2))

    (let ((total-created-in-nodes
            (loop for node in (pooler::distributed-pool-nodes dp)
                  sum (pooler::pool-total-created node))))
      ;; Each of the 3 nodes has threshold 1, so 3 items are created initially.
      ;; Subsequent fetches might create more if keys map to different nodes and items are in use.
      (assert (>= total-created-in-nodes 3)))

    (format t "~&Test 'test-basic-distributed-pool' passed.~%"))
  t)

(defun test-node-management ()
  (reset-test-globals)
  (let* ((node-configs (list (list :name "n1") (list :name "n2")))
         (dp (pooler:make-distributed-pool :name "NodeMgmtPool"
                                           :node-configs node-configs
                                           :item-maker #'make-test-item
                                           :item-destroyer #'destroy-test-item
                                           :default-capacity 3
                                           :default-threshold 1
                                           :replicas 5))) ; Using fewer replicas for predictability in small tests
    (assert (= (length (pooler::distributed-pool-nodes dp)) 2))

    ;; Add a node
    (pooler:add-node-to-distributed-pool dp
                                         (list :name "n3"
                                               :item-maker #'make-test-item
                                               :item-destroyer #'destroy-test-item))
    (assert (= (length (pooler::distributed-pool-nodes dp)) 3))
    (assert (find "n3" (pooler::distributed-pool-nodes dp) :key #'pooler::pool-name :test #'string=))
    ;; Check for n3's replicas in the hash ring
    (assert (<= (pooler::distributed-pool-replicas dp)
                  (count "n3" (pooler::distributed-pool-hash-ring dp)
                         :key #'pooler::hash-ring-node-id :test #'string=)))


    ;; Remove a node
    (pooler:remove-node-from-distributed-pool dp "n1")
    (assert (= (length (pooler::distributed-pool-nodes dp)) 2))
    (assert (not (find "n1" (pooler::distributed-pool-nodes dp) :key #'pooler::pool-name :test #'string=)))
    (assert (not (find "n1" (pooler::distributed-pool-hash-ring dp) :key #'pooler::hash-ring-node-id :test #'string=)))

    ;; Try to remove non-existent node (should error)
    (handler-case (pooler:remove-node-from-distributed-pool dp "n1-nonexistent")
      (pooler::pool-error (c) (assert c) (format t "~&Test 'test-node-management': Caught expected error for non-existent node: ~A~%" c))
      (:no-error (obj) (declare (ignore obj)) (error "Should have signaled a pool-error for removing non-existent node.")))

    (format t "~&Test 'test-node-management' passed.~%"))
  t)

(defun test-key-distribution ()
  (reset-test-globals)
  (let* ((node-configs (list (list :name "dist-node-a")
                             (list :name "dist-node-b")))
         (dp (pooler:make-distributed-pool :name "KeyDistPool"
                                           :node-configs node-configs
                                           :item-maker #'make-test-item
                                           :item-destroyer #'destroy-test-item
                                           :replicas 20 ))) ; Fair number of replicas
    (let ((key-node-map (make-hash-table :test 'equal)))
      (dotimes (i 50) ; Use a decent number of keys
        (let* ((key (format nil "testkey-~A" i))
               (node (pooler:get-node-for-key dp key)))
          (push key (gethash (pooler::pool-name node) key-node-map nil))))

      (assert (= (hash-table-count key-node-map) 2) ()
              "Keys were not distributed to all nodes. Map: ~A" key-node-map)
      (format t "~&Test 'test-key-distribution': Node A got ~A keys, Node B got ~A keys.~%"
              (length (gethash "dist-node-a" key-node-map))
              (length (gethash "dist-node-b" key-node-map)))
      (assert (and (> (length (gethash "dist-node-a" key-node-map)) 0)
                   (> (length (gethash "dist-node-b" key-node-map)) 0)))
      (format t "~&Test 'test-key-distribution' passed.~%")))
  t)

(defun test-error-conditions ()
    (reset-test-globals)
    ;; Test with make-distributed-pool missing item-maker
    (handler-case (pooler:make-distributed-pool :name "ErrorPoolNoMaker"
                                               :node-configs (list (list :name "err-node-no-maker")))
        (pooler::pool-error (c) (assert c) (format t "~&Test 'test-error-conditions': Caught expected error for missing item-maker in make-distributed-pool: ~A~%" c))
        (:no-error (obj) (declare (ignore obj)) (error "Should have signaled pool-error for missing item-maker.")))

    (let ((dp (pooler:make-distributed-pool :name "ErrorPoolMain"
                                            :node-configs (list (list :name "err-node1"
                                                                      :item-maker #'make-test-item
                                                                      :item-destroyer #'destroy-test-item))
                                            )))
        (pooler:remove-node-from-distributed-pool dp "err-node1")
        (handler-case (pooler:fetch-from-distributed-pool dp "somekey")
            (pooler::pool-error (c)
                (assert c)
                (format t "~&Test 'test-error-conditions': Caught expected error for empty ring: ~A~%" c))
            (:no-error (obj) (declare (ignore obj)) (error "Should have signaled pool-error for empty ring.")))

        (handler-case (pooler:add-node-to-distributed-pool dp (list :name "badnode-no-maker"))
             (pooler::pool-error (c) (assert c) (format t "~&Test 'test-error-conditions': Caught expected error for bad node config (add-node): ~A~%" c))
            (:no-error (obj) (declare (ignore obj)) (error "Should have signaled pool-error for bad node config in add-node.")))

        (format t "~&Test 'test-error-conditions' passed.~%"))
    t)

(defun run-all-tests ()
  (format t "~&--- Running Pooler Tests ---~%")
  (let ((passed-all t))
    (handler-case (test-basic-distributed-pool)
        (error (c) (setf passed-all nil) (format t "~&Test 'test-basic-distributed-pool' FAILED: ~A~%" c)))
    (handler-case (test-node-management)
        (error (c) (setf passed-all nil) (format t "~&Test 'test-node-management' FAILED: ~A~%" c)))
    (handler-case (test-key-distribution)
        (error (c) (setf passed-all nil) (format t "~&Test 'test-key-distribution' FAILED: ~A~%" c)))
    (handler-case (test-error-conditions)
        (error (c) (setf passed-all nil) (format t "~&Test 'test-error-conditions' FAILED: ~A~%" c)))

    (if passed-all
        (format t "~&--- All tests completed successfully ---~%")
        (format t "~&--- !!! Some tests FAILED !!! ---~%"))
    passed-all))
