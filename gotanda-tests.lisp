(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*standard-output* (make-broadcast-stream)))
    (require 'asdf)
    (require 'lisp-unit)
    (require 'gotanda)))

;;====================
;; Initialize
;;====================
(in-package :clsql)
(start-transaction)

;; drop all tables and create it again
(dolist (table-str (list-tables))
  (let ((table (intern table-str :got)))
    (drop-table table)
    (create-view-from-class table)))

(in-package :got)
(loop for p in '(define-test assert-eq assert-equal assert-true run-tests)
   do (shadowing-import (concat-symbol-pkg :lisp-unit p)))

;;====================
;; Test Start
;;====================

(define-test task
  (assert-eq nil (find-task :body "Buy Milk"))
  (let ((task (create-task :body "Buy Milk")))
    (assert-true task)
    (assert-equal "Buy Milk" (get-body task))
    (clsql:update-records-from-instance task))
  (let ((task (find-task :body "Buy Milk")))
    (assert-equal 1 (get-id task))
    (assert-equal "Buy Milk" (get-body task))))

;;====================
;; Test End
;;====================

(run-tests)
(clsql:rollback)
