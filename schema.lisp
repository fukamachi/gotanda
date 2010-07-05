(in-package :got)

;; WARN: this is in-memory database
(defvar *db* (clsql:connect '(":memory:") :database-type :sqlite3 :if-exists :old))

(clsql:def-view-class user ()
  ((id
    :db-kind :key
    :db-constraints (:not-null :unique)
    :type integer
    :initarg :id)
   (name
    :accessor get-name
    :type (string 20)
    :initarg :name)
   (pass
    :accessor get-pass
    :type (string 20)
    :initarg :pass))
  (:base-table user))

(clsql:def-view-class task ()
  ((id
    :db-kind :key
    :db-constraints (:not-null :unique)
    :type integer
    :initarg :id)
   (body
    :accessor get-body
    :type (string 1400)
    :initarg :body))
  (:base-table task))

(clsql:create-view-from-class 'user :database *db*)
(clsql:create-view-from-class 'task :database *db*)
