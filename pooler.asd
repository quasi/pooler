;;;; pooler.asd

(asdf:defsystem #:pooler
  :serial t
  :description "A generic pooler library."
  :author "Abhijit Rao"
  :license "MIT"
  :depends-on (#-sbcl :bordeaux-threads #+sbcl :sb-concurrency)
  :components ((:file "package")
	       (:file "queue")
               (:file "pooler")))

