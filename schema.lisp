(in-package :got)

(defvar *db* (clsql:connect '("gotan.db") :database-type :sqlite3 :if-exists :old))

(clsql:def-view-class task ()
  ((id
    :db-kind :key
    :db-constraints (:not-null :unique)
    :type integer
    :initarg :id
    :initform nil)
   (body
    :accessor get-body
    :db-constraints (:not-null)
    :type (string 1400)
    :initarg :body)
   (deadline
    :accessor get-deadline
    :type clsql:date
    :initform nil
    :initarg :deadline)
   (tags
    :accessor get-tags
    :type list
    :initarg :tags
    :initform nil))
  (:base-table task))

(defun table-exists-p (table)
  (clsql:select 'name
                :from (clsql:sql-expression :table 'sqlite_master)
                :where (clsql:sql-operation '= (clsql:sql-expression :attribute 'name) table)))
(or (table-exists-p "TASK") (clsql:create-view-from-class 'task :database *db*))
