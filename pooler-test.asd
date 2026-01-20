;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;
;;;  Pooler Test System Definition
;;;

(asdf:defsystem #:pooler-test
  :description "Test suite for the pooler library"
  :author "quasi <quasi@quasilabs.in>"
  :license "MIT"
  :depends-on (#:pooler
               #:fiveam)
  :serial t
  :components ((:file "tests/pooler-test"))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :fiveam :run! :pooler-test)))
