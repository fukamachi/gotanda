(in-package :got)

(defmacro def-create (table args)
  `(defun ,(concat-symbol 'create- table) (&key ,@args)
     (make-instance ',table
                    ,@(flatten
                       (loop for x in args
                             collect (list (concat-symbol-pkg :KEYWORD x) x))))))

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

(def-create task (body))
(def-find task (id body))
