(in-package :got)

(defun build-where (&rest args)
  (apply #'clsql:sql-operation 'and
         (loop for (k v) in args
            unless (null v)
            collect (clsql:sql-operation
                     '=
                     (clsql:sql-expression :attribute
                                           (concat-symbol-pkg :KEYWORD k)) v))))

(defmacro def-find (table args)
  `(defun ,(concat-symbol 'find- table) (&key ,@args)
     (clsql:select ,@(mapcar #'(lambda (a) `(quote ,a)) args)
                   :from (clsql:sql-expression :table ',table)
                   :where (build-where ,@(mapcar #'(lambda (a) `(list ',a ,a)) args)))))

(defun parse-tags (body)
  (remove-duplicates
   (cl-ppcre:all-matches-as-strings "(?:(?<=\\W)|(?<=^))#\\w+" body)
   :test #'string=))

(defun get-tag (name)
  (aif (clsql:select 'tag :where (clsql:sql-operation '= 'name name) :limit 1 :flatp t)
       (car it)
       (progn
         (clsql:insert-records :into 'tag :av-pairs `((name ,name)))
         (get-tag name))))

(defun get-tag-id (name)
  (car (clsql:select 'id :from 'tag :where (clsql:sql-operation '= 'name name) :flatp t)))

(defun create-task (&key body deadline)
  (let* ((tags (mapcar #'get-tag (parse-tags body))))
    (make-instance 'task :body body :deadline deadline :tags (mapcar #'get-id tags))))

(def-find task (id body))

(defun list-task (&key tag)
  (let ((tasks (clsql:select 'task :flatp t)))
    (if tag
        (let ((tag-id (get-tag-id tag)))
          (remove-if-not #'(lambda (task) (member tag-id (get-tags task))) tasks))
        tasks)))
