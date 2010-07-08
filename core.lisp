(in-package :got)

(defun build-where (&rest args)
  (apply #'clsql:sql-operation 'and
         (loop for (k v) in (group args 2)
            unless (null v)
            collect (clsql:sql-operation '= (intern (string k)) v))))

(defmacro select-one (table &rest args)
  `(car (clsql:select ',table :where (build-where ,@args) :limit 1 :flatp t)))

(defun parse-tags (body)
  (remove-duplicates
   (cl-ppcre:all-matches-as-strings "(?:(?<=\\W)|(?<=^))#\\w+" body)
   :test #'string=))

(defun get-tag (name)
  (aif (select-one tag :name name)
       it
       (progn
         (clsql:insert-records :into 'tag :av-pairs `((name ,name)))
         (get-tag name))))

(defun get-tag-id (name)
  (car (clsql:select 'id :from 'tag :where (clsql:sql-operation '= 'name name) :flatp t)))

(defun create-task (&key body deadline)
  (let* ((tags (mapcar #'get-tag (parse-tags body))))
    (make-instance 'task :body body :deadline deadline :tags (mapcar #'get-id tags))))

(defun filter-by-tag (tag tasks)
  (if tag
      (let ((tag-id (get-tag-id tag)))
        (remove-if-not #'(lambda (task) (member tag-id (get-tags task))) tasks))
      tasks))

(defun filter-by-deadline (deadline tasks)
  (if deadline
      (remove-if-not #'(lambda (task)
                         (if (listp deadline)
                             (apply (car deadline) (cadr deadline))
                             (member deadline (get-deadline task)))) tasks)
      tasks))

(defun list-task (&key tag deadline)
  (let ((tasks (clsql:select 'task :flatp t)))
    (aand (filter-by-tag tag tasks)
          (filter-by-deadline deadline it))))
