(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*standard-output* (make-broadcast-stream)))
    (require 'asdf)
    (require 'lisp-unit)
    (require 'cl-interpol)
    (require 'gotanda)))
(cl-interpol:enable-interpol-syntax)
(clsql:enable-sql-reader-syntax)

;;====================
;; Initialize
;;====================
(in-package :clsql)
(got:initialize-database)
(start-transaction)

;; drop all tables and create it again
(dolist (table-str (list-tables))
  (let ((table (intern table-str :got)))
    (drop-table table)
    (create-view-from-class table)))

(in-package :got)
(loop for p in '(define-test run-tests
                 assert-true assert-eq assert-equal assert-equality)
   do (shadowing-import (intern (symbol-name p) :lisp-unit)))

;;====================
;; Test Start
;;====================

(defmacro assert-time (form &rest args)
  `(assert-equality #'clsql:time=
                    ,form
                    (clsql:make-time
                     ,@(loop for arg in args
                           for label in '(:year :month :day :hour :minute :second)
                           until (null arg)
                           append (list label arg)))))

(define-test str->date
  (assert-time (str->date "2003-04-07") 2003 4 7)
  (assert-time (str->date "2003-4-7") 2003 4 7)
  (assert-time (str->date "2003-21-32") 2003 21 32)
  (assert-time (str->date "1987-10-3 18:11:29") 1987 10 3 18 11 29))

(define-test split-params
  (assert-equal '("create" "task" "--body" "Buy Milk #shopping")
                (split-params "create task --body \"Buy Milk #shopping\""))
  (assert-equal '("create task" "--body" "Buy Milk")
                (split-params "create\\ task --body \"Buy Milk\""))
  (assert-equal '("create" "task") (split-params #?"  create\t task  \n"))
  (assert-equal '("list" "--tag" "#shopping") (split-params "list --tag \"#shopping\""))
  (assert-equal '("list" "nil" "< 2010-04-07") (split-params "list nil \"< 2010-04-07\"")))

(define-test create-task
  (assert-eq nil (select-one task :body "Buy Milk #shopping"))
  (let ((task (create-task :body "Buy Milk #shopping")))
    (assert-true task)
    (assert-equal "Buy Milk #shopping" (get-body task)))
  (let ((task (select-one task :body "Buy Milk #shopping")))
    (assert-true task)
    (assert-equal 1 (get-id task))
    (assert-equal "Buy Milk #shopping" (get-body task))))

(define-test edit-task
  (let ((task (select-one task)))
    (edit-task task :body "Editted")))

(define-test list-task
  (create-task :body "Produce Astro Boy #invent" :deadline "2003-04-07")
  (assert-true (list-task :tag "#invent"))
  (assert-true (list-task :tag t :deadline (list '< (clsql:get-time))))
  (assert-true (list-task :tag t :deadline (list '> (clsql:make-time)))))

;;====================
;; Test End
;;====================

(run-tests)
(clsql:rollback)
