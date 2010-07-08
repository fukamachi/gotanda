(in-package :got)

(defvar *db* (clsql:connect '("gotan.db") :database-type :sqlite3 :if-exists :old))

(clsql:def-view-class task ()
  ((id
    :db-kind :key
    :db-constraints (:not-null :unique)
    :type integer
    :initarg :id
    :initform nil
    :accessor get-id)
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
    :initform nil)
   (tag-id
    :accessor get-tag-id
    :db-kind :join
    :db-info (:join-class tag
              :home-key tags
              :foreign-key id
              :set nil)))
  (:base-table task))

(clsql:def-view-class tag ()
  ((id
    :db-kind :key
    :db-constraints (:not-null :unique)
    :type integer
    :initarg :id
    :initform nil
    :accessor get-id)
   (name
    :db-constraints (:not-null :unique)
    :type string
    :initarg :name
    :accessor get-name))
  (:base-table tag))

(defun table-exists-p (table)
  (clsql:select 'name
                :from (clsql:sql-expression :table 'sqlite_master)
                :where (clsql:sql-operation '= (clsql:sql-expression :attribute 'name) table)))
(or (table-exists-p "TASK") (clsql:create-view-from-class 'task :database *db*))
(or (table-exists-p "TAG") (clsql:create-view-from-class 'tag :database *db*))
