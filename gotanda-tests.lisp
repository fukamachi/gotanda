(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*standard-output* (make-broadcast-stream)))
    (require 'asdf)
    (require 'lisp-unit)
    (require 'gotanda)))

(in-package :lisp-unit)
(define-test find-task
  (assert-equal nil (got:find-task :body "Buy Milk")))
(run-tests)
