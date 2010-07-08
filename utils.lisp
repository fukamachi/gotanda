(in-package :got)

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

(defun concat-symbol-pkg (pkg &rest args)
  (declare (dynamic-extent args))
  (flet ((stringify (arg)
           (etypecase arg
             (string
              (string-upcase arg))
             (symbol
              (symbol-name arg)))))
    (let ((str (apply #'concatenate 'string (mapcar #'stringify args))))
      (nth-value 0 (intern str (if pkg pkg *package*))))))

(defun concat-symbol (&rest args)
  (apply #'concat-symbol-pkg nil args))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun group (source n)
  (if (not (listp source)) (error "group: not list"))
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                 (rec rest (cons
                             (subseq source 0 n)
                             acc))
                 (nreverse
                   (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                        (car x)
                        (rec (cdr x) acc))))))
    (rec x nil)))

(defun take (num list)
  (loop repeat num for x in list collect x))

(defun take-until (pred list)
  (labels ((rec (pred list acc)
                (if (or (null (car list)) (funcall pred (car list)))
                    (values acc list)
                  (rec pred (cdr list) (nconc acc (list (car list)))))))
    (rec pred list nil)))

(defun split-with (sep string)
  (loop for i = 0 then (1+ j)
        as j = (position sep string :start i)
        collect (subseq string i j)
        while j))

;;==================
;; For debug
;;==================
(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro print-form-and-results (form)
  `(format t "~&~A --> ~S~%" (write-to-string ',form) ,form))
