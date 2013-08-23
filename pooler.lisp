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
    :documentation "Method to make a new pool item")
   (pool-item-destroyer
    :accessor pool-item-destroyer
    :initform #'(lambda (item) (setf item nil))
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


;;; the pool-item-maker should return a vaild pool item or NIL
(defgeneric new-pool-item (pool))

(defmethod new-pool-item ((pool pool))
  (funcall (pool-item-maker pool)))

(defgeneric destroy-pool-item (pool pool-item))

(defmethod destroy-pool-item ((pool pool) pool-item)
  (funcall (pool-item-destroyer pool) pool-item))


(defun pool-grow (pool grow-by)
  (loop for x from 1 to grow-by
     do (bordeaux-threads:with-lock-held ((pool-lock pool))
	  (let ((pool-item (new-pool-item pool)))
	    (if (and pool-item
		     (< (current-size pool) (max-capacity pool)))
		(progn
		  (enqueue (queue pool) pool-item)
		  (incf (total-created pool))
		  (incf (current-size pool))))))))


(defgeneric fetch-from (pool))

(defmethod fetch-from ((pool pool))
  "Fetches a pool item from pool."
  (bordeaux-threads:with-lock-held ((pool-lock pool))
    (if (queue-empty-p (queue pool))
	(pool-error "Pool Empty" :pool-name (name pool))
	(progn
	  (decf (current-size pool))
	  (incf (total-uses pool))
	  (dequeue (queue pool))))))


(defgeneric return-to (pool pool-item))

(defmethod return-to ((pool pool) pool-item)
  "Returns a pool object to the pool"
  (bordeaux-threads:with-lock-held ((pool-lock pool))
    (when (< (current-size pool) (max-capacity pool))
      (progn
	(enqueue (queue pool) pool-item)
	(incf (current-size pool))))))


(defun fetch-from+ (pool &key (tries 3))
  "Tries a couple of times to fetch from pool. Grows the pool."
  (loop for x from 1 to tries
     do (let ((item (handler-case (fetch-from pool)
		      (pool-error () nil))))
	  (if (not item)
	      (pool-grow pool 1)
	      (return item)))))


(defun pool-init (pool)
  "Cleans up the pool & reinits it with POOL-SIZE number of POOL-ITEM"
  (loop for item = (handler-case (fetch-from pool) (pool-error () nil)) while item
     do (bordeaux-threads:with-lock-held ((pool-lock pool))
	  (destroy-pool-item pool item)
	  (decf (current-size pool))))
  (setf (queue pool) (make-queue)
	(current-size pool) 0
	(total-uses pool) 0
	(total-created pool) 0)
  (pool-grow pool (min-threshold pool)))


(defmacro with-pool ((pool-item pool) &body body)
  `(let (,pool-item)
     (unwind-protect
	  (progn
	    (setf ,pool-item (fetch-from+ ,pool))
	    ,@body)
      (when ,pool-item
	(return-to ,pool ,pool-item)))))



;;; EOF
