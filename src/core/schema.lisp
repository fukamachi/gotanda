(in-package :got)

(clsql:def-view-class task ()
  ((id
    :db-kind :key
    :accessor get-id
    :db-constraints (:not-null :unique)
    :type integer)
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
    :initform nil)
   (tag-id
    :db-kind :join
    :db-info (:join-class tag
              :home-key tags
              :foreign-key id
              :set nil)))
  (:base-table task))

(clsql:def-view-class tag ()
  ((id
    :db-kind :key
    :accessor get-id
    :db-constraints (:not-null :unique)
    :type integer
    :initarg :id)
   (name
    :accessor get-name
    :db-constraints (:not-null :unique)
    :type string
    :initarg :name))
  (:base-table tag))

(clsql:def-view-class history ()
  ((id
    :db-kind :key
    :accessor get-id
    :db-constraints (:not-null :unique)
    :type integer
    :initarg :id
    :initform nil)
   (target
    :accessor get-target
    :db-constraints (:not-null)
    :type symbol
    :initarg :target)
   (field
    :accessor get-field
    :db-constraints (:not-null)
    :type symbol
    :initarg :target)
   (action
    :accessor get-action
    :db-constraints (:not-null)
    :type keyword
    :initarg :action)
   (date
    :accessor get-date
    :type clsql:wall-time
    :initform (clsql:get-time)))
  (:base-table history))

(defun initialize-database ()
  ;; create tables if it does not exist
  (defvar *db*
    (clsql:connect
     `(,(namestring
         (asdf:system-relative-pathname (asdf:find-system :gotanda) "gotan.db")))
     :database-type :sqlite3
     :if-exists :old))
  (dolist (table '(task tag))
    (or (clsql:table-exists-p (symbol-name table))
        (clsql:create-view-from-class table))))
