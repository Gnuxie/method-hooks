#+sbcl (setf sb-ext:*derive-function-types* t)

(ql:register-local-projects)
(ql:quickload '(:method-hooks-test :uiop))
(method-hooks-test:ci-run) ; ci-run should quit before we do
