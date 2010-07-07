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

(defun create-task (&key body deadline)
  (let ((tags (parse-tags body)))
    (make-instance 'task :body body :deadline deadline :tags tags)))

(def-find task (id body))
