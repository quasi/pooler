(in-package #:pooler)


;;;
;;; queue implementation from http://aima.cs.berkeley.edu/lisp/utilities/queue.lisp
;;;

(defstruct q
  (key #'identity)
  (last nil)
  (elements nil))

(defun make-empty-queue ()
  (make-q))

(defun empty-queue? (q)
  "Are there no elements in the queue?"
  (= (length (q-elements q)) 0))

(defun queue-front (q)
  "Return the element at the front of the queue."
  (elt (q-elements q) 0))

(defun remove-front (q)
  "Remove the element from the front of the queue and return it."
  (if (listp (q-elements q))
      (pop (q-elements q))
    nil))


(defun enqueue-at-end (q items)
  "Add a list of items to the end of the queue."
  ;; To make this more efficient, keep a pointer to the last cons in the queue
  (let ((items (list items)))
    (cond ((null items) nil)
	  ((or (null (q-last q)) (null (q-elements q)))
	   (setf (q-last q) (last items)
		 (q-elements q) (nconc (q-elements q) items)))
	  (t (setf (cdr (q-last q)) items
		   (q-last q) (last items))))))

;; the wrappers

(defun make-queue ()
  ""
  #+sbcl (sb-concurrency:make-queue)
  #+allegro (make-instance 'mp:queue)
  #-(or sbcl allegro) (make-empty-queue))

(defmacro enqueue (queue what)
  ""
  #+sbcl `(sb-concurrency:enqueue ,what ,queue)
  #+allegro `(mp:enqueue ,queue ,what)
  #-(or sbcl allegro) `(enqueue-at-end ,queue ,what))

(defmacro dequeue (queue)
  ""
  #+sbcl `(sb-concurrency:dequeue ,queue)
  #+allegro `(mp:dequeue ,queue)
  #-(or sbcl allegro) `(remove-front ,queue))

(defmacro queue-empty-p (queue)
  ""
  #+sbcl `(sb-concurrency:queue-empty-p ,queue)
  #+allegro `(mp:queue-empty-p ,queue)
  #-(or sbcl allegro) `(empty-queue? ,queue))


;;; Couple of Lock wrappers
(defun make-pool-lock ()
  #+sbcl (sb-thread:make-mutex :name "Pool Lock")
  #-sbcl (bordeaux-threads:make-lock "Pool Lock"))

(defmacro with-pool-lock ((lock) &body body)
  #+sbcl `(sb-thread:with-mutex (,lock) ,@body)
  #-sbcl `(bordeaux-threads:with-lock-held (,lock) ,@body))

;;; EOF
