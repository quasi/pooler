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
    :initarg :name
    :reader name
    :initform "Connection Pool"
    :type simple-string
    :documentation "Name of this pool")
   (pool
    :initform (make-queue)
    :accessor pool)
   (pool-lock
    :reader pool-lock
    :initform (bordeaux-threads:make-lock "Pool Lock"))
   (max-capacity
    :initarg :max-capacity
    :reader max-capacity
    :initform 4
    :type fixnum
    :documentation "Total capacity of the pool to hold pool objects")
   (current-size
    :accessor current-size
    :type fixnum
    :initform 0)
   (total-uses
    :accessor total-uses
    :initform 0
    :type fixnum
    :documentation "Total uses of the pool")
   (total-created
    :accessor total-created
    :initform 0
    :type fixnum
    :documentation "Total pool objects created")
   (pool-grow-requests
    :initform 0
    :accessor pool-grow-requests
    :type fixnum
    :documentation "Pool Grow Request pending Action")
   (pool-grow-lock
    :initform (bordeaux-threads:make-lock "Pool Grow Lock")
    :reader pool-grow-lock))
  (:documentation "The POOL object"))


(defmethod print-object ((p pool) stream)
  (format stream "#<~S ~a ~d:~d>"
	  (type-of p)
	  (name p)
	  (max-capacity p)
	  (current-size p)))


;;; EOF
