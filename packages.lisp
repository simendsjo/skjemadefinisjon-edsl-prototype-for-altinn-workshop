(cl:defpackage :app
  (:use :cl :alexandria :serapeum)
  (:local-nicknames (#:a #:alexandria)
                    (#:t #:trivia)
                    (#:f #:fset)
                    (#:ps #:parenscript))
  (:export #:start-server
           #:define-app
           #:define-field
           #:define-type))
