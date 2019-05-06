#+sbcl (setf sb-ext:*derive-function-types* t)

(ql:register-local-projects)
(ql:quickload :method-hooks-test)
(method-hooks-test:ci-run)
