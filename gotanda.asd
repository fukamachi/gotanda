(defsystem gotanda
  :version "1.0.0-SNAPSHOT"
  :author "Eitarow Fukamachi"
  :depends-on (:clsql :cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "schema")
               (:file "core")))
