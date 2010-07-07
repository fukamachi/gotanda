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
    :type datetime
    :initform nil
    :initarg :deadline)
   (tags
    :accessor get-tags
    :type list
    :initarg :tags
    :initform '(this is test))
   (tag_id
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
    :initform nil)
   (task_id
    :accessor get-task-id
    :db-constraints (:not-null)
    :type integer
    :initarg :task-id)
   (name
    :accessor get-name
    :db-constraints (:not-null)
    :type (string 20)
    :initarg :name))
  (:base-table tag))

(defun table-exists-p (table)
  (clsql:select 'name
                :from (clsql:sql-expression :table 'sqlite_master)
                :where (clsql:sql-operation '= (clsql:sql-expression :attribute 'name) table)))
(or (table-exists-p "TASK") (clsql:create-view-from-class 'task :database *db*))
(or (table-exists-p "TAG") (clsql:create-view-from-class 'tag :database *db*))
