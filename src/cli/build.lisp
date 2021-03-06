(require 'asdf)
(require 'gotanda)
(load (merge-pathnames "color.lisp" *load-pathname*))

(in-package :got)

(cl-interpol:enable-interpol-syntax)
(defvar *list-items* nil)

(defun make-keyword (symb)
  (values (intern (string-upcase symb) :KEYWORD)))

(setf (symbol-function 'dispatch)
      (alet ((actions (make-hash-table)))
            (dlambda
             (:define (name def)
                 (setf (gethash name actions) def))
             (:do (name &rest args)
                  (aif (gethash name actions)
                       (apply it args)
                       (funcall this :do :error "Undefined action: ~a" name))))))

(defmacro define-action (name args &body body)
  `(dispatch :define (make-keyword ',name) (lambda ,args ,@body)))

(define-action all ()
  (dispatch :do :tag t))

(define-action tag (&optional (tag nil) deadline)
  (setf *list-items*
        (list-task :tag tag
                   :deadline (and deadline
                                  (destructuring-bind (compare-fn datestr)
                                      (cl-ppcre:split #\Space deadline)
                                    (list (intern compare-fn) (str->date datestr))))))
  (loop for task in *list-items*
     for i = 1 then (1+ i)
     do (format t
                #?"${(color:style (:gray) i)}: ${(color:style (:cyan) (get-body task))} ~@\[[~a]~]~%"
                (aand (get-deadline task) (color:style (:gray) it)))))

(define-action create (&optional body deadline)
  (let ((task (create-task
               :body (or body (prompt-read "Body?>"))
               :deadline (or deadline (prompt-read "Deadline?>")))))
    (format t "New Task: ~a~%" (get-body task))))

(defmacro! define-action/index (name vars args &body body)
  (let ((idx (getf vars :index (gensym)))
        (var (getf vars :var (gensym))))
    `(define-action ,name (,g!index ,@args)
       (let* ((,idx (- (parse-integer ,g!index) 1))
              (,var (nth ,idx *list-items*)))
         (case ,var
           ((NIL) (dispatch :do :error "Out of range."))
           ((DELETED) (dispatch :do :error "It is already deleted."))
           (t ,@body))))))

(define-action/index edit (:var task) (&optional body deadline)
  (edit-task task
             :body (or body (prompt-read "Body?>"))
             :deadline (or deadline (prompt-read "Deadline?>"))))

(define-action/index delete (:var task :index idx) ()
  (delete-task task)
  (setf (nth idx *list-items*) 'DELETED))

(define-action/index finish (:var task) ()
  (finish-task task))

(define-action action-for (n)
  (destructuring-bind (action &rest args)
      (loop for params = (split-params (prompt-read "What action?>"))
         if params return params)
    (let ((action-kwd (make-keyword action)))
      (if (member action-kwd '(:edit :delete :finish))
          (apply #'dispatch :do action-kwd n args)
          (dispatch :do :error "Invalid action")))))

(define-action error (form &rest args)
  (apply #'format t #?"${form}~%" args))

(defun run-dispatch (args)
  (cond
    ((#~m/^\d+$/ (car args)) (push :action-for args))
    ((#~m/#\w+/ (car args)) (push :tag args)))
  (apply #'dispatch :do (make-keyword (car args)) (cdr args)))

(defun main ()
  (initialize-database)
  (loop for input = (prompt-read ">")
     with clsql:*default-caching* = nil
     until (eq nil input)
     unless (string= "" input)
     do (run-dispatch (split-params input))))

(sb-ext:save-lisp-and-die "got" :toplevel #'main :executable t)
