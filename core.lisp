(in-package :got)

(enable-read-macros)

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

(defun create-tag-if-not-exists (name)
  (aif (select-one tag :name name)
       it
       (progn
         (clsql:insert-records :into 'tag :av-pairs `((name ,name)))
         (create-tag-if-not-exists name))))

(defun get-tag-id (name)
  (car (clsql:select 'id :from 'tag :where (clsql:sql-operation '= 'name name) :flatp t)))

(defun get-tag-ids (&rest names)
  (clsql:select 'id :from 'tag :where (clsql:sql-operation 'in 'name names) :flatp t))

(defun update-task (task &key body deadline)
  (let ((tag-names (parse-tags body)))
    (dolist (tag tag-names) (create-tag-if-not-exists tag))
    (setf (slot-value task 'body) body)
    (setf (slot-value task 'deadline) (str->date deadline))
    (setf (slot-value task 'tags) (get-tag-ids tag-names))
    (clsql:update-records-from-instance task)
    task))

(defun create-task (&key body deadline)
  (update-task (make-instance 'task) :body body :deadline deadline))

(defun edit-task (task &key body deadline)
  (update-task task :body body :deadline deadline))

(defmacro filter-> (target &rest by)
  `(aand ,target
         ,@(mapcar #`(if ,$1 (,(symb 'filter-by- $1) ,$1 it) it) by)))

(defun filter-by-tag (tag tasks)
  (let ((tag-id (get-tag-id tag)))
    (remove-if-not #^(member tag-id (get-tags $1)) tasks)))

(defun filter-by-deadline (deadline tasks)
  (let ((compare-fn (intern (mkstr 'time (car deadline)) :clsql)))
    (remove-if-not #'(lambda (task)
                       (and (get-deadline task)
                            (funcall compare-fn
                                     (get-deadline task) (cadr deadline))))
                   tasks)))

(defun list-task (&key tag deadline)
  (filter-> (clsql:select 'task :flatp t)
            tag deadline))
