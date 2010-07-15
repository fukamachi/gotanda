(require 'asdf)
(require 'hunchentoot)
(require 'gotanda)

(in-package :hunchentoot)
(start (make-instance 'hunchentoot:acceptor :port 8080))
(setf *hunchentoot-default-external-format* (flex:make-external-format :utf-8 :eof-style :lf))
(setf *default-content-type* "text/html; charset=utf-8")

(define-easy-handler (say-yo :uri "/yo") (name)
  (format nil "Hey~@[ ~A~]!" name))
