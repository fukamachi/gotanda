(require 'hunchentoot)
(require 'cl-who)
(require 'gotanda)

(in-package :cl-user)

(defpackage gotanda-web
  (:use #:cl #:hunchentoot #:cl-who #:gotanda)
  (:import-from #:hunchentoot define-easy-handler)
  (:import-from #:cl-who with-html-output-to-string htm)
  (:import-from #:gotanda))

(in-package :gotanda-web)

(got:initialize-database)
(setf hunchentoot:*hunchentoot-default-external-format* (flex:make-external-format :utf-8 :eof-style :lf))
(setf hunchentoot:*default-content-type* "text/html; charset=utf-8")
(setf hunchentoot:*show-lisp-errors-p* t)
(setf clsql:*default-caching* nil) ;; FIXME: this would let the performance low

(defmacro defpage (name property &body body)
  (declare (ignore name))
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (htm
      (:head (:title (str ,(getf property :title)))) ,@body)))

(defun create-form/ (stream)
  (with-html-output (stream)
    (:form :method "POST" :action "/create"
           (:input :type "text" :name "body")
           (:input :type "submit" :value "Add"))))

(defun task/ (stream body deadline)
  (with-html-output (stream)
    (:b (str body))
    (if deadline (htm (:span " [" (str deadline) "]")))
    :br))

(defun task-list/ (stream tasks)
  (with-html-output (stream)
    (loop for task in tasks
       do (task/ stream (get-body task) (get-deadline task)))
    (create-form/ stream)))

(defun list-view (&key tag deadline)
  (with-html-output (*standard-output*)
    (task-list/
     *standard-output*
     (list-task
      :tag tag
      :deadline (and deadline
                     (destructuring-bind (compare-fn datestr)
                         (cl-ppcre:split #\Space deadline)
                       (list (intern compare-fn) (str->date datestr))))))))

(define-easy-handler (all :uri "/") ()
  (defpage tag-list (:title "All Tasks")
    (let ((clsql:*default-caching* nil))
      (list-view :tag t))))

(define-easy-handler (tag :uri "/tag") (name deadline)
  (defpage all-tasks-list (:title (format nil "Tag: ~:[[none]~;~:*~a~]" name))
    (list-view :tag name :deadline deadline)))

(define-easy-handler (create :uri "/create") (body)
  (defpage create-request ()
    (create-task :body body)
    (hunchentoot:redirect "/")))

(defvar *server* (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 8080)))
