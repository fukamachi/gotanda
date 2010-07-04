(in-package :cl-user)

(defpackage gotanda
  (:nicknames got)
  (:use #:cl)
  (:export #:create-user #:create-task #:find-user #:find-task))
