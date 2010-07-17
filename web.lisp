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

(defun list-view (&key tag deadline)
  (let ((tasks (list-task
                :tag tag
                :deadline (and deadline
                               (destructuring-bind (compare-fn datestr)
                                   (cl-ppcre:split #\Space deadline)
                                 (list (intern compare-fn) (str->date datestr)))))))
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (loop for task in tasks
           for body = (get-body task)
           for deadline = (get-deadline task)
         do (htm
             (:b (str body))
             (if deadline (htm (:span " [" (str deadline) "]")))
             :br)))))

(define-easy-handler (all :uri "/") () (list-view :tag t))

(define-easy-handler (tag :uri "/tag") (name deadline)
  (list-view :tag name :deadline deadline))

(defvar *server* (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 8080)))
