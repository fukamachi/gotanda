(in-package :got)

(defmacro def-create (table args)
  `(defun ,(concat-symbol 'create- table) (&key ,@args)
     (make-instance ',table
                    ,@(flatten
                       (loop for x in args
                             collect (list (concat-symbol-pkg :KEYWORD x) x))))))

(defmacro build-where (&rest args)
  `(clsql:sql-operation
    'and
    ,@(loop for (k v) in args
            unless (null v)
            collect `(clsql:sql-operation '=
                                          (clsql:sql-expression :attribute ,k) ,v))))

(defmacro def-find (table args)
  `(defun ,(concat-symbol 'find- table) (&key ,@args)
     (clsql:select ,@(mapcar #'(lambda (a) `(quote ,a)) args)
                   :from (clsql:sql-expression :table ',table)
                   :where (build-where ,@(loop for x in args collect (list (concat-symbol-pkg :KEYWORD x) x))))))

(def-create user (name pass))
(def-create task (body))
(def-find user (id name pass))
(def-find task (id body))
