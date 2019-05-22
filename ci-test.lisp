#+sbcl (setf sb-ext:*derive-function-types* t)

(ql:register-local-projects)
(ql:quickload '(:method-hooks-test :uiop))
(unwind-protect (method-hooks-test:ci-run) ; ci-run should quit before we do
  (uiop:quit -1))                          ; we have to do this for implementations that don't have --quit or disable debugger
