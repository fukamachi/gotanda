(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (require 'asdf)
    (require 'lisp-unit)
    (require 'gotanda)))

(in-package :lisp-unit)
(define-test find-user
  (assert-equal nil (got:find-user :name "fukamachi")))
(run-tests)
