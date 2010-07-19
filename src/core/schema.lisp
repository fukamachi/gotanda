(in-package :got)

(clsql:def-view-class task ()
  ((id
    :db-kind :key
    :accessor get-id
    :db-constraints (:not-null :unique)
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
   (tags
    :accessor get-tags
    :type list
    :initarg :tags
    :initform nil)))

(clsql:def-view-class history (task)
  ((id
    :db-kind :key
    :accessor get-id
    :db-constraints (:not-null :unique)
    :type integer
    :initarg :id
    :initform nil)
   (task-id
    :accessor get-task-id
    :type integer
    :initarg :task-id)
   (task-info
    :accessor get-task
    :db-kind :join
    :db-info (:join-class task
              :home-key task-id
              :foreign-key id))
   (action
    :accessor get-action
    :db-constraints :not-null
    :type string
    :initarg :action)
   (fields
    :accessor get-fields
    :type list
    :initarg :fields
    :initform nil)
   (timestamp
    :accessor get-timestamp
    :type universal-time
    :initform (get-universal-time))))

(defun initialize-database ()
  ;; create tables if it does not exist
  (defvar *db*
    (clsql:connect
     `(,(namestring
         (asdf:system-relative-pathname (asdf:find-system :gotanda) "gotan.db")))
     :database-type :sqlite3
     :if-exists :old))
  (or (clsql:table-exists-p "TASK") (clsql:create-view-from-class 'task))
  (when (not (clsql:table-exists-p "HISTORY"))
    (clsql:create-view-from-class 'history)
    (clsql:create-index 'timestamp-index :on 'history
                        :attributes '(timestamp))))
