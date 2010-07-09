(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*standard-output* (make-broadcast-stream)))
    (require 'asdf)
    (require 'lisp-unit)
    (require 'cl-interpol)
    (require 'gotanda)))
(cl-interpol:enable-interpol-syntax)

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
(loop for p in '(define-test run-tests
                 assert-true assert-eq assert-equal assert-equality)
   do (shadowing-import (concat-symbol-pkg :lisp-unit p)))

;;====================
;; Test Start
;;====================

(defmacro assert-time (form &rest args)
  `(assert-equality #'clsql:time=
                    ,form
                    (clsql:make-time
                     ,@(flatten
                        (loop for arg in args
                           for label in '(:year :month :day :hour :minute :second)
                           until (null arg)
                           collect (list label arg))))))

(define-test str->date
  (assert-time (str->date "2003-04-07") 2003 4 7)
  (assert-time (str->date "2003-4-7") 2003 4 7)
  (assert-time (str->date "2003-21-32") 2003 21 32)
  (assert-time (str->date "1987-10-3 18:11:29") 1987 10 3 18 11 29))

(define-test split-params
  (assert-equal '("create" "task" "--body" "Buy Milk")
                (split-params "create task --body \"Buy Milk\""))
  (assert-equal '("create task" "--body" "Buy Milk")
                (split-params "create\\ task --body \"Buy Milk\""))
  (assert-equal '("create" "task") (split-params #?"  create\t task  \n"))
  (assert-equal '("list" "--tag" "#shopping") (split-params "list --tag \"#shopping\"")))

(define-test create-task
  (assert-eq nil (select-one task :body "Buy Milk"))
  (let ((task (create-task :body "Buy Milk")))
    (assert-true task)
    (assert-equal "Buy Milk" (get-body task)))
  (let ((task (select-one task :body "Buy Milk")))
    (assert-true task)
    (assert-equal 1 (get-id task))
    (assert-equal "Buy Milk" (get-body task))))

(define-test list-task
  (create-task :body "Produce Astro Boy #invent" :deadline "2003-04-07")
  (assert-true (list-task :tag "#invent"))
  (assert-true (list-task :deadline (list '< (clsql:get-time))))
  (assert-true (list-task :deadline (list '> (clsql:make-time)))))

;;====================
;; Test End
;;====================

(run-tests)
(clsql:rollback)
