(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*standard-output* (make-broadcast-stream)))
    (require 'asdf)
    (require 'lisp-unit)
    (require 'gotanda)))

(in-package :got)
;; use in-memory database
(setq *db* (clsql:connect '(":memory:") :database-type :sqlite3 :if-exists :old))
(clsql:create-view-from-class 'task :database *db*)

(in-package :lisp-unit)

(define-test task
  (assert-eq nil (got:find-task :body "Buy Milk"))
  (let ((task (got:create-task :body "Buy Milk")))
    (assert-true task)
    (assert-equal "Buy Milk" (got:get-body task))
    (clsql:update-records-from-instance task))
  (assert-equal '((1 "Buy Milk")) (got:find-task :body "Buy Milk")))

(run-tests)
