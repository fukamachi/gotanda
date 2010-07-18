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

(defun create-tag-if-not-exists (name-dirty)
  (let ((name (string-downcase name-dirty)))
    (aif (select-one tag :name name)
         it
         (progn
           (clsql:insert-records :into 'tag :av-pairs `((name ,name)))
           (select-one tag :name name)))))

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

(defun delete-task (task)
  (clsql:delete-instance-records task))

(defun delete-task-by-id (id)
  (delete-task (select-one task :id id)))

(defun finish-task (task)
  (setf (finished-p task) t)
  (clsql:update-records-from-instance task))

(defun filter-by-tag (tag tasks)
  (cond
    ((eq t tag) tasks)
    ((eq nil tag) (remove-if #^(get-tags $1) tasks))
    (t (let ((tag-id (get-tag-id tag)))
         (remove-if-not #^(member tag-id (get-tags $1)) tasks)))))

(defun filter-by-deadline (deadline tasks)
  (if deadline
      (let ((compare-fn (intern (mkstr 'time (car deadline)) :clsql)))
        (remove-if-not #'(lambda (task)
                           (and (get-deadline task)
                                (funcall compare-fn
                                         (get-deadline task) (cadr deadline))))
                       tasks))
      tasks))

(defun list-task (&key tag deadline)
  (aand (clsql:select 'task :flatp t)
        (remove-if #'finished-p it)
        (filter-by-tag tag it)
        (filter-by-deadline deadline it)))
