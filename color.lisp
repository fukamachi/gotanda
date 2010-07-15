(in-package :cl-user)

(defpackage color
  (:use #:cl)
  (:export #:style))

(in-package :color)

(defun make-pairs (list)
  (if (null list) list
      (destructuring-bind (x y &rest rest) list
        (cons `(,x ,y) (make-pairs rest)))))

(defun read-hash (stream char)
  (declare (ignore char))
  (let ((argslist (read-delimited-list #\} stream t))
        (hash (make-hash-table :test #'equal)))
    (loop for (key value) in (make-pairs argslist)
         do (setf (gethash key hash) (eval value)))
    hash))

(set-macro-character #\{ #'read-hash)
(set-macro-character #\} (get-macro-character #\)))

(defvar *current-styles* nil)
(defvar *color-code*
  {:bold 1
   :italic 3
   :underline 4
   :strikethrough 9
   :fore
    {:black 30
     :red 31
     :green 32
     :yellow 33
     :blue 34
     :magenta 35
     :cyan 36
     :gray 37
     :default 39}
   :back
    {:black 40
     :red 41
     :yellow 43
     :blue 44
     :magenta 45
     :cyan 46
     :gray 47
     :default 49}})

(defun get-code (style)
  (unless (and (listp style)
               (member style '(:bold :italic :underline :strikethrough)))
    (setf style (list :fore style)))
  (format nil "~C[~dm"
          #\Esc
          (let ((code (gethash
                       (if (listp style) (car style) style) *color-code*)))
            (if (hash-table-p code)
                (gethash (cadr style) code)
                code))))

(defmacro style (style-names text)
  `(concatenate 'string
                (let ((*current-styles* (apply #'concatenate 'string (mapcar #'get-code ',style-names))))
                  (format nil "~a~a~C[0m" *current-styles* ,text #\Esc))
                (if *current-styles* (format nil "~a" *current-styles*))))
