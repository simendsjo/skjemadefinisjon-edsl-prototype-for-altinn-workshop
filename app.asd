(defsystem "app"
  :description "app"
  :author "Simen Endsjø <simendsjo@gmail.com>"
  :version "0.1.0"
  :depends-on ("bordeaux-threads"
               "uuid"
               "alexandria"
               "serapeum"
               "trivia"
               "fset"
               "cl-fad"
               "hunchentoot"
               "shasht"
               "parenscript"
               "spinneret"
               "spinneret/ps"
               "spinneret/cl-markdown")
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "app")
               (:file "server")
               (:file "demos")))
