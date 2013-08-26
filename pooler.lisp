;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; pooler.lisp

(in-package #:pooler)



;;; Error Conditions

(define-condition pool-error (error)
  ((message
    :initarg :message
    :accessor message
    :initform nil)
   (pool-name
    :initarg :pool-name
    :accessor pool-name
    :initform nil))
  (:documentation "Superclass for all errors related to Pooler."))

(define-condition pool-item-creation-error (pool-error) ())
(define-condition pool-empty-error (pool-error) ())

(defun pool-error (message &key pool-name)
  "Signals an error of type POOL-ERROR with the provided information"
  (error 'pool-error
	 :message message
	 :pool-name pool-name))

;;;
;;;
;;; Pool
;;;
;;;

(defclass pool ()
  ((name
    :reader   name
    :initform "Default Pool"
    :initarg  :name
    :type     simple-string
    :documentation "Name of this pool")
   (queue
    :accessor queue
    :initform (make-queue))
   (pool-lock
    :reader   pool-lock
    :initform (bordeaux-threads:make-lock "Pool Lock"))
   (pool-item-maker
    :accessor pool-item-maker
    :initform #'(lambda () 'SAMPLE-ITEM)
    :initarg :pool-item-maker
    :documentation "Method to make a new pool item")
   (pool-item-destroyer
    :accessor pool-item-destroyer
    :initform #'(lambda (item) (setf item nil))
    :initarg :pool-item-destroyer
    :documentation "Method to sanely destroy a pool item")
   (max-capacity
    :reader   max-capacity
    :initform 4
    :initarg  :max-capacity
    :type     fixnum
    :documentation "Total capacity of the pool to hold pool objects")
   (min-threshold
    :initarg  :min-threshold
    :accessor min-threshold
    :initform 2
    :type     fixnum
    :documentation "Minimum idle items")
   (current-size
    :accessor current-size
    :type     fixnum
    :initform 0)
   (total-uses
    :accessor total-uses
    :initform 0
    :type     fixnum
    :documentation "Total uses of the pool")
   (total-created
    :accessor total-created
    :initform 0
    :type     fixnum
    :documentation "Total pool objects created"))
  (:documentation "The POOL object"))

(defmethod print-object ((p pool) stream)
  (format stream "#<~S ~a Max:~d Current:~d >"
	  (type-of p)
	  (name p)
	  (max-capacity p)
	  (current-size p)))



(defun new-pool (&key name (pool-item-maker #'(lambda () 'SAMPLE-ITEM)) (pool-item-destroyer #'(lambda (item) (setf item nil))) (max-capacity 4) (min-threshold 2))
  (make-instance 'pool
		 :name name
		 :pool-item-maker pool-item-maker
		 :pool-item-destroyer pool-item-destroyer
		 :max-capacity max-capacity
		 :min-threshold min-threshold))


;;; the pool-item-maker should return a vaild pool item or NIL
(defgeneric new-pool-item (pool))

(defmethod new-pool-item ((pool pool))
  (handler-case (funcall (pool-item-maker pool))
    (error () (error 'pool-item-creation-error :pool-name (name pool)))))

(defgeneric destroy-pool-item (pool pool-item))

(defmethod destroy-pool-item ((pool pool) pool-item)
  (ignore-errors (funcall (pool-item-destroyer pool) pool-item)))


(defun grow-pool (pool &optional grow-by)
  (let ((grow-by (or grow-by (min-threshold pool))))
    (loop for x from 1 to grow-by
       do (bordeaux-threads:with-lock-held ((pool-lock pool))
	    (when (< (current-size pool) (max-capacity pool))
	      (let ((pool-item (new-pool-item pool)))
		(when pool-item
		  (enqueue (queue pool) pool-item)
		  (incf (total-created pool))
		  (incf (current-size pool)))))))))


(defgeneric fetch-from (pool))

(defmethod fetch-from ((pool pool))
  "Fetches a pool item from pool."
  (bordeaux-threads:with-lock-held ((pool-lock pool))
    (when (not (queue-empty-p (queue pool)))
      (decf (current-size pool))
      (incf (total-uses pool))
      (dequeue (queue pool)))))


(defun fetch-from+ (pool &key (tries 3))
  "Tries a couple of times to fetch from pool. Grows the pool."
  (loop for x from 1 to tries
     do (let ((item (fetch-from pool)))
	  (if (not item)
	      (grow-pool pool)
	      (return item)))))


(defgeneric return-to (pool pool-item))

(defmethod return-to ((pool pool) pool-item)
  "Returns a pool object to the pool"
  (bordeaux-threads:with-lock-held ((pool-lock pool))
    (if (< (current-size pool) (max-capacity pool))
	(progn
	  (enqueue (queue pool) pool-item)
	  (incf (current-size pool)))
	(destroy-pool-item pool pool-item))))


(defun pool-init (pool)
  "Cleans up the pool & reinits it with POOL-SIZE number of POOL-ITEM"
  (loop for item = (handler-case (fetch-from pool) (pool-error () nil))
     while item
     do (bordeaux-threads:with-lock-held ((pool-lock pool))
	  (destroy-pool-item pool item)
	  (decf (current-size pool))))
  (bordeaux-threads:with-lock-held ((pool-lock pool))
    (setf (queue pool) (make-queue)
	  (current-size pool) 0
	  (total-uses pool) 0
	  (total-created pool) 0))
  (grow-pool pool))


(defmacro with-pool ((pool-item pool) &body body)
  `(let (,pool-item)
     (unwind-protect
	  (progn
	    (setf ,pool-item (fetch-from+ ,pool))
	    ,@body)
      (when ,pool-item
	(return-to ,pool ,pool-item)))))



;;; EOF
