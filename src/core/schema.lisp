(in-package :got)

(clsql:def-view-class task ()
  ((id
    :db-kind :key
    :accessor get-id
    :db-constraints (:not-null :unique)
    :type integer
    :initform nil)
   (global-id
    :db-constraints :unique
    :type integer
    :initform nil)
   (body
    :accessor get-body
    :type (string 1400)
    :void-value ""
    :initarg :body)
   (deadline
    :accessor get-deadline
    :type clsql:wall-time
    :initarg :deadline)
   (finished-p
    :accessor finished-p
    :type boolean
    :initform nil)
   (deleted-p
    :accessor deleted-p
    :type boolean
    :initform nil)
   (tags
    :accessor get-tags
    :type list
    :initarg :tags
    :initform nil)
   (created-at
    :type integer
    :initform (get-universal-time))
   (updated-at
    :type integer
    :initform (get-universal-time))))

(defun initialize-database ()
  ;; create tables if it does not exist
  (defvar *db*
    (clsql:connect
     `(,(namestring
         (asdf:system-relative-pathname (asdf:find-system :gotanda) "gotan.db")))
     :database-type :sqlite3
     :if-exists :old))
  (or (clsql:table-exists-p "TASK") (clsql:create-view-from-class 'task)))
