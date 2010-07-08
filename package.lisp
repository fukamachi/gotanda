(in-package :cl-user)

(defpackage gotanda
  (:nicknames got)
  (:use #:cl)
  (:export #:create-task #:find-task #:list-task))
