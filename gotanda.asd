(defsystem gotanda
  :version "1.0.0-SNAPSHOT"
  :author "Eitarow Fukamachi"
  :depends-on (:clsql :cl-ppcre :cl-interpol)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "schema")
               (:file "core")))
