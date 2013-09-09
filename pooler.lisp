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

(defun pool-error (message &key pool-name)
  "Signals an error of type POOL-ERROR with the provided information"
  (error 'pool-error
	 :message message
	 :name pool-name))

;;;
;;;
;;; Pool
;;;
;;;

(defstruct (pool (:constructor make-pool (&key name capacity threshold item-maker item-destroyer)))
  (name "Default Pool" :type simple-string :read-only t)
  (queue (make-queue))
  (lock (make-pool-lock) :read-only t)
  (item-maker #'(lambda () 'SAMPLE-ITEM) :type function :read-only t)
  (item-destroyer #'(lambda (item) (setf item nil)) :type function :read-only t)
  (capacity 4 :type fixnum)
  (threshold 2 :type fixnum)
  (current-size 0 :type fixnum)
  (total-uses 0 :type fixnum)
  (total-created 0 :type fixnum))



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
		  (incf (pool-current-size pool)))))))))



(defun fetch-from (pool)
  "Fetches a pool item from pool."
  (with-pool-lock ((pool-lock pool))
    (when (not (queue-empty-p (pool-queue pool)))
      (decf (pool-current-size pool))
      (incf (pool-total-uses pool))
      (dequeue (pool-queue pool)))))


(defun fetch-from+ (pool &key (tries 3))
  "Tries a couple of times to fetch from pool. Grows the pool."
  (loop for x from 1 to tries
     do (let ((item (fetch-from pool)))
	  (if (not item)
	      (grow-pool pool)
	      (return item)))))



(defun return-to (pool pool-item)
  "Returns a pool object to the pool"
  (with-pool-lock ((pool-lock pool))
    (if (< (pool-current-size pool) (pool-capacity pool))
	(progn
	  (enqueue (pool-queue pool) pool-item)
	  (incf (pool-current-size pool)))
	(destroy-pool-item pool pool-item))))


(defun pool-init (pool)
  "Cleans up the pool & reinits it with MIN-THRESHOLD number of POOL-ITEM"
  (loop for item = (handler-case (fetch-from pool) (pool-error () nil))
     while item
     do (with-pool-lock ((pool-lock pool))
	  (destroy-pool-item pool item)
	  (decf (pool-current-size pool))))
  (with-pool-lock ((pool-lock pool))
    (setf (pool-queue pool) (make-queue)
	  (pool-current-size pool) 0
	  (pool-total-uses pool) 0
	  (pool-total-created pool) 0))
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
