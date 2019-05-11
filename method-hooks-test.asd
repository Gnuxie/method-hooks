(asdf:defsystem "method-hooks-test"
  :author "Gnuxie <Gnuxie@protonmail.com>"
  :depends-on ("parachute" "uiop" "method-hooks")
  :components ((:module "test"
                        :components
                        ((:file "test-package")
                         (:file "test")
                         (:file "run-test")
                         (:file "environment-test")))))
