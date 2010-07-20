(in-package :got)

(enable-read-macros)

(defun build-where (&rest args)
  (if args
      (apply #'clsql:sql-operation 'and
             (loop for (k v) in (group args 2)
                unless (null v)
                collect (clsql:sql-operation '= (intern (string k)) v)))))

(defmacro! select-one (table &rest args)
  `(let ((,g!plist '(:limit 1 :flatp t))
         (,g!where (apply #'build-where (list ,@args))))
     (if ,g!where (setf ,g!plist (append ,g!plist (list :where ,g!where))))
     (car (apply #'clsql:select ',table ,g!plist))))

(defun parse-tags (body)
  (remove-duplicates
   (cl-ppcre:all-matches-as-strings "(?:(?<=\\W)|(?<=^))#\\w+" body)
   :test #'string=))

(defun create-task (&key body deadline)
  (let* ((id (1+ (or (caar (clsql:query "SELECT MAX(ID) FROM TASK")) 0)))
         (task (make-instance 'task
                              :id id
                              :body body
                              :tags (parse-tags body)
                              :deadline (str->date deadline))))
    (clsql:update-records-from-instance task)
    task))

(defmacro update-task (task &optional slots)
  `(progn
     (setf (slot-value ,task 'updated-at) (get-universal-time))
     ,(if slots
          `(clsql:update-record-from-slots ,task (cons 'updated-at ,slots))
          `(clsql:update-records-from-instance ,task))))

(defun edit-task (task &key body deadline)
  (let (fields)
    (unless (string= body (slot-value task 'body))
      (setf (slot-value task 'body) body)
      (setf (slot-value task 'tags) (parse-tags body))
      (push 'body fields))
    (unless (cond ((eq nil deadline) (eq nil (slot-value task 'deadline)))
                  ((eq nil (slot-value task 'deadline)) nil)
                  (t (clsql:time= (slot-value task 'deadline) deadline)))
      (setf (slot-value task 'deadline) (str->date deadline))
      (push 'deadline fields))
    (if fields
        (update-task task fields)))
  task)

(defun delete-task (task)
  (setf (slot-value task 'deleted-p) t)
  (update-task task '(deleted-p)))

(defun delete-task-by-id (id)
  (delete-task (select-one task :id id)))

(defun finish-task (task)
  (setf (finished-p task) t)
  (update-task task '(finished-p)))

(defun filter-by-tag (tag tasks)
  (cond
    ((eq t tag) tasks)
    ((eq nil tag) (remove-if #^(get-tags $1) tasks))
    (t (remove-if-not #^(member tag (get-tags $1) :test #'string=) tasks))))

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
        (remove-if #'deleted-p it)
        (filter-by-tag tag it)
        (filter-by-deadline deadline it)))
