;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; pooler.lisp

(in-package #:pooler)



;;; Error Conditions

(define-condition pool-error (error)
  ((message
    :initarg :message
    :accessor message
    :initform nil)
   (name
    :initarg :name
    :accessor name
    :initform nil))
  (:documentation "Superclass for all errors related to Pooler."))

(define-condition pool-item-creation-error (pool-error) ())
(define-condition pool-empty-error (pool-error) ())

(define-condition pool-warning (warning)
  ((message
    :initarg :message
    :accessor message
    :initform nil))
  (:report (lambda (condition stream)
             (format stream "~a" (message condition)))))

(defun pool-error (message &key pool-name)
  "Signals an error of type POOL-ERROR with the provided information"
  (error 'pool-error
	 :message message
	 :name pool-name))


;;;
;;; The 'POOL' Structure
;;;
;;; capacity - max number of pool-items a pool can have. This affects the concurrency.
;;; threshold - number of idle connections which a pool should maintain
;;; timeout - number of seconds of no use of pool before it is re-inited
;;; last-acccess - last access timestamp
;;;

(defstruct (pool (:constructor make-pool (&key name capacity threshold item-maker item-destroyer timeout)))
  (name "Default Pool" :type simple-string :read-only t)
  (queue (make-queue))
  (lock (make-pool-lock) :read-only t)
  (item-maker #'(lambda () 'SAMPLE-ITEM) :type function :read-only t)
  (item-destroyer #'(lambda (item) (setf item nil)) :type function :read-only t)
  (capacity 40 :type fixnum)
  (threshold 2 :type fixnum)
  (timeout 300 :type fixnum)
  (last-access 0 :type integer)
  (current-size 0 :type fixnum)
  (total-uses 0 :type fixnum)
  (total-created 0 :type fixnum)
  (total-pool-inits 0 :type fixnum))

(defstruct (distributed-pool (:constructor make-distributed-pool-internal))
  (name "Default Distributed Pool" :type simple-string :read-only t)
  (nodes nil :type list) ; List of regular pool objects
  (hash-ring nil :type (or null list)) ; Sorted list of hash-ring-node objects
  (replicas 10 :type fixnum) ; Number of virtual nodes per actual node
  (lock (make-pool-lock) :read-only t))

(defstruct hash-ring-node
  (id nil)          ; Identifier of the actual node/pool
  (hash-value 0 :type integer)) ; Hash value on the ring

(defun internal-string-hash (str)
  "Simple string hashing function. Replace with a more robust one if needed."
  (sxhash str))

(defun add-node-to-ring (dist-pool node-id)
  "Adds a node (with its replicas) to the hash ring."
  (let ((ring (distributed-pool-hash-ring dist-pool))
        (replicas (distributed-pool-replicas dist-pool)))
    (dotimes (i replicas)
      (let* ((replica-id (format nil "~A-~A" node-id i))
             (hash-value (internal-string-hash replica-id)))
        (push (make-hash-ring-node :id node-id :hash-value hash-value) ring)))
    (setf (distributed-pool-hash-ring dist-pool)
          (sort ring #'< :key #'hash-ring-node-hash-value))
    dist-pool))

(defun remove-node-from-ring (dist-pool node-id)
  "Removes a node (and all its replicas) from the hash ring."
  (setf (distributed-pool-hash-ring dist-pool)
        (delete-if #'(lambda (hr-node) (equal (hash-ring-node-id hr-node) node-id))
                   (distributed-pool-hash-ring dist-pool)))
  dist-pool))

(defun get-node-id-for-key-from-ring (dist-pool key)
  "Gets the ID of the node responsible for the given key from the hash ring."
  (let* ((ring (distributed-pool-hash-ring dist-pool))
         (key-hash (internal-string-hash key)))
    (unless ring
      (pool-error "Hash ring is empty." :pool-name (distributed-pool-name dist-pool)))
    ;; Find the first node in the sorted ring whose hash is >= key-hash
    (let ((found-node (find-if #'(lambda (hr-node) (>= (hash-ring-node-hash-value hr-node) key-hash))
                               ring)))
      (if found-node
          (hash-ring-node-id found-node)
          ;; If key-hash is greater than all node hashes, wrap around to the first node
          (hash-ring-node-id (first ring))))))

(defun make-distributed-pool (&key name (node-configs nil) (replicas 10) (item-maker nil) (item-destroyer nil) (default-capacity 40) (default-threshold 2) (default-timeout 300))
  "Creates and returns a new DISTRIBUTED-POOL.
NODE-CONFIGS should be a list of plists, where each plist defines a node.
Each node config plist can have: :name, :capacity, :threshold, :timeout, :item-maker, :item-destroyer.
If item-maker or item-destroyer are not provided in node-configs, the top-level
item-maker and item-destroyer are used. Similarly for capacity, threshold, and timeout."
  (let* ((dist-pool-name (or name "Default Distributed Pool"))
         (dist-pool (make-distributed-pool-internal :name dist-pool-name :replicas replicas)))
    (setf (distributed-pool-nodes dist-pool)
          (loop for node-conf in node-configs
                for i from 0
                collect (let* ((node-name (getf node-conf :name (format nil "~A-Node-~A" dist-pool-name i)))
                               (node-item-maker (or (getf node-conf :item-maker) item-maker))
                               (node-item-destroyer (or (getf node-conf :item-destroyer) item-destroyer))
                               (node-capacity (getf node-conf :capacity default-capacity))
                               (node-threshold (getf node-conf :threshold default-threshold))
                               (node-timeout (getf node-conf :timeout default-timeout)))
                          (unless node-item-maker
                            (pool-error "ITEM-MAKER must be provided either globally or for each node."
                                        :pool-name node-name))
                          (make-pool :name node-name
                                     :capacity node-capacity
                                     :threshold node-threshold
                                     :item-maker node-item-maker
                                     :item-destroyer node-item-destroyer
                                     :timeout node-timeout))))
    (dolist (node (distributed-pool-nodes dist-pool))
      (add-node-to-ring dist-pool (pool-name node)))
    dist-pool))

(defun get-node-for-key (dist-pool key)
  "Retrieves the actual pool object (node) for a given key from the distributed pool."
  (with-pool-lock ((distributed-pool-lock dist-pool)) ; Ensure thread-safe access to the ring/nodes
    (let ((node-id (get-node-id-for-key-from-ring dist-pool key)))
      (unless node-id
        (pool-error "Could not find a node ID for the key in the hash ring."
                    :pool-name (distributed-pool-name dist-pool)))
      (let ((found-node (find node-id (distributed-pool-nodes dist-pool)
                              :key #'pool-name
                              :test #'equal)))
        (unless found-node
          (pool-error (format nil "Node with ID '~A' found in ring but not in node list." node-id)
                      :pool-name (distributed-pool-name dist-pool)))
        found-node))))

(defun fetch-from-distributed-pool (dist-pool key &key (tries 2))
  "Fetches an item from the distributed pool based on the key.
The key determines which underlying node (pool) is used.
TRIES is passed to the underlying node's FETCH-FROM."
  (let ((node (get-node-for-key dist-pool key)))
    (unless node
      ;; This case should ideally be caught by get-node-for-key,
      ;; but as a safeguard:
      (pool-error (format nil "No node found for key '~A' in ~A."
                          key (distributed-pool-name dist-pool))
                  :pool-name (distributed-pool-name dist-pool)))
    (fetch-from node :tries tries)))

(defun return-to-distributed-pool (dist-pool key item)
  "Returns an item to the distributed pool based on the key.
The key determines which underlying node (pool) the item is returned to."
  (let ((node (get-node-for-key dist-pool key)))
    (unless node
      ;; This case should ideally be caught by get-node-for-key,
      ;; but as a safeguard:
      (pool-error (format nil "No node found for key '~A' in ~A when returning item."
                          key (distributed-pool-name dist-pool))
                  :pool-name (distributed-pool-name dist-pool)))
    (return-to node item)))

(defun add-node-to-distributed-pool (dist-pool node-config)
  "Adds a new node to the distributed pool.
NODE-CONFIG is a plist similar to those in `make-distributed-pool`'s :node-configs.
It must contain at least :name and :item-maker (or rely on a global default if that was a design choice, though explicit is better here).
Returns the modified distributed-pool."
  (with-pool-lock ((distributed-pool-lock dist-pool))
    (let* ((node-name (getf node-config :name))
           (item-maker (getf node-config :item-maker))
           ;; TODO: Consider if we need to access global defaults like in make-distributed-pool
           ;; For now, assume item-maker and item-destroyer are provided or error
           (item-destroyer (getf node-config :item-destroyer))
           (capacity (getf node-config :capacity 40)) ; Default values
           (threshold (getf node-config :threshold 2))
           (timeout (getf node-config :timeout 300)))

      (unless (and node-name item-maker)
        (pool-error "Node config must include :name and :item-maker."
                    :pool-name (distributed-pool-name dist-pool)))

      (when (find node-name (distributed-pool-nodes dist-pool) :key #'pool-name :test #'equal)
        (pool-error (format nil "Node with name '~A' already exists in ~A."
                            node-name (distributed-pool-name dist-pool))
                    :pool-name (distributed-pool-name dist-pool)))

      (let ((new-pool (make-pool :name node-name
                                 :capacity capacity
                                 :threshold threshold
                                 :item-maker item-maker
                                 :item-destroyer item-destroyer
                                 :timeout timeout)))
        (push new-pool (distributed-pool-nodes dist-pool))
        (add-node-to-ring dist-pool node-name)
        dist-pool))))

(defun remove-node-from-distributed-pool (dist-pool node-name-to-remove)
  "Removes a node from the distributed pool by its name.
NOTE: This version does not handle draining items from the pool being removed.
Items in the removed pool will be orphaned until its `pool-init` or manual cleanup.
A more robust implementation might involve item migration or graceful shutdown.
Returns the modified distributed-pool."
  (with-pool-lock ((distributed-pool-lock dist-pool))
    (let ((node-to-remove (find node-name-to-remove (distributed-pool-nodes dist-pool)
                                :key #'pool-name :test #'equal)))
      (unless node-to-remove
        (pool-error (format nil "Node with name '~A' not found in ~A."
                            node-name-to-remove (distributed-pool-name dist-pool))
                    :pool-name (distributed-pool-name dist-pool)))

      ;; Potentially call (pool-init node-to-remove) to destroy its items
      ;; For now, we just remove it from the list and the ring.
      ;; (pool-init node-to-remove) ;; Uncomment if items should be destroyed

      (setf (distributed-pool-nodes dist-pool)
            (delete node-to-remove (distributed-pool-nodes dist-pool)))
      (remove-node-from-ring dist-pool node-name-to-remove)
      dist-pool))))



;;; API

(defun new-pool-item (pool)
  "Creates a new POOL-ITEM using the item-maker funciton stored in the pool"
  (handler-case (funcall (pool-item-maker pool))
    (error () (error 'pool-item-creation-error :pool-name (pool-name pool)))))


(defun destroy-pool-item (pool pool-item)
  "Destroys the POOL-ITEM using the item-destroyer funciton stored in the pool"
  (ignore-errors (funcall (pool-item-destroyer pool) pool-item)))


(defun grow-pool (pool &optional grow-by)
  (let ((grow-by (or grow-by (pool-threshold pool))))
    (loop for x from 1 to grow-by
       do (with-pool-lock ((pool-lock pool))
	    (when (< (pool-current-size pool) (pool-capacity pool))
	      (let ((pool-item (new-pool-item pool)))
		(when pool-item
		  (enqueue (pool-queue pool) pool-item)
		  (incf (pool-total-created pool))
		  (incf (pool-current-size pool))
		  (setf (pool-last-access pool) (get-universal-time)))))))))



(defun fetch-from-aux (pool &optional old)
  "Fetches a pool item from pool."
  (with-pool-lock ((pool-lock pool))
    (cond ((queue-empty-p (pool-queue pool)) nil)
	  ((and (not old)
                (> (get-universal-time)
                   (+ (pool-last-access pool) (pool-timeout pool))))
           :old)
	  (t
	   (decf (pool-current-size pool))
	   (incf (pool-total-uses pool))
	   (setf (pool-last-access pool) (get-universal-time))
	   (dequeue (pool-queue pool))))))


(defun fetch-from (pool &key (tries 2))
  "Tries a couple of times to fetch from pool. Grows the pool."
  (loop for x from 1 to tries
     do (let ((item (fetch-from-aux pool)))
	  (cond ((null item)
		 (grow-pool pool))
		((eq item :old)
		 (pool-init pool))
		(t
		 (return item))))))



(defun return-to (pool pool-item)
  "Returns a pool object to the pool"
  (with-pool-lock ((pool-lock pool))
    (if (< (pool-current-size pool) (pool-capacity pool))
	(progn
	  (enqueue (pool-queue pool) pool-item)
	  (setf (pool-last-access pool) (get-universal-time))
	  (incf (pool-current-size pool)))
	(destroy-pool-item pool pool-item)))) ; we dont want to update last access here


(defun pool-init (pool)
  "Cleans up the pool & reinits it with MIN-THRESHOLD number of POOL-ITEM"
  (loop for item = (handler-case (fetch-from-aux pool t) (pool-error () nil))
     while item
     do (with-pool-lock ((pool-lock pool))
	  (destroy-pool-item pool item)
	  (decf (pool-current-size pool))))
  (with-pool-lock ((pool-lock pool))
    (setf (pool-queue pool) (make-queue)
	  (pool-current-size pool) 0
	  (pool-total-uses pool) 0
	  (pool-total-created pool) 0)
    (incf (pool-total-pool-inits pool))
    (warn 'pool-warning :message (format nil "~a POOL-INIT no.~a" (pool-name pool) (pool-total-pool-inits pool))))
  (grow-pool pool))


(defmacro with-pool ((pool-item pool) &body body)
  `(let (,pool-item)
     (unwind-protect
	  (progn
	    (setf ,pool-item (fetch-from ,pool))
	    ,@body)
      (when ,pool-item
	(return-to ,pool ,pool-item)))))


;;; EOF
