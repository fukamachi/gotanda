(defsystem gotanda
  :version "1.0.0-SNAPSHOT"
  :author "Eitarow Fukamachi"
  :depends-on (:clsql :cl-ppcre :cl-interpol)
  :serial t
  :components ((:file "src/core/package")
               (:file "src/core/utils")
               (:file "src/core/schema")
               (:file "src/core/core")))
