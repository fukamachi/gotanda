(in-package :got)
(cl-interpol:enable-interpol-syntax)

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun flatten (x)
    (labels ((rec (x acc)
               (cond ((null x) acc)
                     ((atom x) (cons x acc))
                     (t (rec
                         (car x)
                         (rec (cdr x) acc))))))
      (rec x nil)))

  (defun g!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "G!"
                  :start1 0
                  :end1 2)))

  (defun o!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "O!"
                  :start1 0
                  :end1 2)))

  (defun o!-symbol-to-g!-symbol (s)
    (symb "G!"
          (subseq (symbol-name s) 2)))

  (defmacro defmacro/g! (name args &body body)
    (let ((symbs (remove-duplicates
                  (remove-if-not #'g!-symbol-p
                                 (flatten body)))))
      `(defmacro ,name ,args
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
                symbs)
           ,@body))))

  (defmacro defmacro! (name args &body body)
    (let* ((os (remove-if-not #'o!-symbol-p args))
           (gs (mapcar #'o!-symbol-to-g!-symbol os)))
      `(defmacro/g! ,name ,args
         `(let ,(mapcar #'list (list ,@gs) (list ,@os))
            ,(progn ,@body)))))

  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg
                 collect (symb '$ i))
       ,(funcall
         (get-macro-character #\`) stream nil)))

  (defun |#^-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg collect (symb '$ i))
       ,(read stream t nil t)))

  (defun segment-reader (stream ch n)
    (if (> n 0)
        (let ((chars))
          (do ((curr (read-char stream)
                     (read-char stream)))
              ((char= ch curr))
            (push curr chars))
          (cons (coerce (nreverse chars) 'string)
                (segment-reader stream ch (- n 1))))))

  (defmacro! match-mode-ppcre-lambda-form (o!args)
    ``(lambda (,',g!str)
        (cl-ppcre:scan
         ,(car ,g!args)
         ,',g!str)))

  (defmacro! subst-mode-ppcre-lambda-form (o!args)
    ``(lambda (,',g!str)
        (cl-ppcre:regex-replace-all
         ,(car ,g!args)
         ,',g!str
         ,(cadr ,g!args))))

  (defun |#~-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let ((mode-char (read-char stream)))
      (cond
        ((char= mode-char #\m)
         (match-mode-ppcre-lambda-form
          (segment-reader stream
                          (read-char stream)
                          1)))
        ((char= mode-char #\s)
         (subst-mode-ppcre-lambda-form
          (segment-reader stream
                          (read-char stream)
                          2)))
        (t (error "Unknown #~~ mode character"))))))

(defmacro enable-read-macros ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-dispatch-macro-character #\# #\` #'|#`-reader|)
     (set-dispatch-macro-character #\# #\^ #'|#^-reader|)
     (set-dispatch-macro-character #\# #\~ #'|#~-reader|)))

(enable-read-macros)

(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
          (lambda (d)
            `(,(if (eq t (car d))
                   t
                   (list (car d)))
               (apply (lambda ,@(cdr d))
                      ,(if (eq t (car d))
                           g!args
                           `(cdr ,g!args)))))
          ds))))

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

;; TODO: refactor
(defun str->date (str)
  (cl-ppcre:register-groups-bind ((#'parse-integer year) (#'parse-integer month) (#'parse-integer day) (#'parse-integer hour) (#'parse-integer minute) (#'parse-integer second))
      ("(\\d{4})-(\\d{1,2})-(\\d{1,2})(?: (\\d{1,2}):(\\d{1,2}):(\\d{1,2}))?" str)
      (clsql:make-time :year year :month month :day day :hour (or hour 0) :minute (or minute 0) :second (or second 0))))

(defun prompt-read (prompt)
  (format *query-io* "~a " prompt)
  (force-output *query-io*)
  (read-line *query-io* nil))

(defun split-params (param-str)
  (remove-if #^(string= "" $1)
    (cl-ppcre:split #?"\0"
      (aand param-str
            (#~s/\"(.+?)\s+([^\"]+?)\"/\1\\ \2/ it)
            (#~s/\"// it)
            (cl-ppcre:regex-replace-all #?/(?<!\\)\s/ it #?"\0")
            (#~s/\\(\s)/\1/ it)))))

;;==================
;; For debug
;;==================
(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro print-form-and-results (form)
  `(format t "~&~A --> ~S~%" (write-to-string ',form) ,form))

(defmacro dis (args &body body)
  `(disassemble
    (compile nil
      (lambda ,(mapcar (lambda (a)
                         (if (consp a)
                             (cadr a)
                             a))
                       args)
        (declare
         ,@(mapcar
            #`(type ,(car $1) ,(cadr $1))
            (remove-if-not #'consp args)))
        ,@body))))
