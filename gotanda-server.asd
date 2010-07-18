(defsystem gotanda-server
    :version "1.0.0-SNAPSHOT"
    :author "Eitarow Fukamachi"
    :depends-on (:hunchentoot :cl-who :clsql :cl-ppcre :gotanda)
    :serial t
    :components ((:file "src/server/package")
                 (:file "src/server/web")))
