(asdf:defsystem "cl-ipp"
  :description "Internet Printing Protocol (RFC 8010/8011) client: binary codec, request builders, HTTP transport."
  :author "David Awad"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("usocket")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "tags")
               (:file "codec")
               (:file "requests")
               (:file "simplify")
               (:file "http")
               (:file "client"))
  :in-order-to ((test-op (test-op "cl-ipp/test"))))

(asdf:defsystem "cl-ipp/test"
  :description "FiveAM tests for cl-ipp."
  :license "MIT"
  :depends-on ("cl-ipp" "fiveam" "usocket")
  :serial t
  :pathname "test"
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (op c)
             (let ((result (uiop:symbol-call :fiveam :run :cl-ipp-tests)))
               (uiop:symbol-call :fiveam :explain! result)
               (unless (uiop:symbol-call :fiveam :results-status result)
                 (error "cl-ipp tests failed")))))
