(in-package :method-hooks-environment-test)

(define-test environment-test
  :parent (:method-hooks-test method-hooks-test)
  :depends-on ((:method-hooks-test comprehensive-test))

  ;; see https://common-lisp.net/project/asdf/uiop.html#UIOP_002fRUN_002dPROGRAM uiop:run-program returns 3 values.

  (flet ((safe-filename-command (file-name)
           (format nil "sbcl --load ~a --disable-debugger --quit"
                   (asdf:system-relative-pathname :method-hooks file-name))))
    (is = 0 (nth-value 2 (uiop:run-program (safe-filename-command "test/compile-again.lisp"))))
    (let ((exit-code (nth-value 2 (uiop:run-program (safe-filename-command "test/test-newenv.lisp")))))
      (is = 0 exit-code))))

