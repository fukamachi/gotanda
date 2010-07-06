(in-package :got)

(defvar *db* (clsql:connect '("gotan.db") :database-type :sqlite3 :if-exists :old))

(clsql:def-view-class task ()
  ((id
    :db-kind :key
    :db-constraints (:not-null :unique)
    :type integer
    :initform nil
    :initarg :id)
   (body
    :accessor get-body
    :type (string 1400)
    :initarg :body))
  (:base-table task))

(defun table-exists-p (table)
  (clsql:select 'name
                :from (clsql:sql-expression :table 'sqlite_master)
                :where (clsql:sql-operation '= (clsql:sql-expression :attribute 'name) table)))
(or (table-exists-p "TASK") (clsql:create-view-from-class 'task :database *db*))
