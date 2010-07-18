(in-package :got)

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
    :type (string 1400)
    :initform ""
    :initarg :body)
   (deadline
    :accessor get-deadline
    :type clsql:wall-time
    :initform nil
    :initarg :deadline)
   (finished-p
    :accessor finished-p
    :type boolean
    :initform nil)
   (tags
    :accessor get-tags
    :type list
    :initarg :tags
    :initform nil)))

(defun initialize-database ()
  ;; create tables if it does not exist
  (defvar *db*
    (clsql:connect
     `(,(namestring
         (asdf:system-relative-pathname (asdf:find-system :gotanda) "gotan.db")))
     :database-type :sqlite3
     :if-exists :old))
  (dolist (table '(task))
    (or (clsql:table-exists-p (symbol-name table))
        (clsql:create-view-from-class table))))
