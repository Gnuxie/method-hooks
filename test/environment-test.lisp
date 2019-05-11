(in-package :method-hooks-environment-test)

(define-test environment-test
  :parent (:method-hooks-test method-hooks-test)
  :depends-on ((:method-hooks-test comprehensive-test))
  
  (compile-file (asdf:system-relative-pathname :method-hooks "test/test.lisp"))
  (compile-file (asdf:system-relative-pathname :method-hooks "test/test-package.lisp"))

  ;; see https://common-lisp.net/project/asdf/uiop.html#UIOP_002fRUN_002dPROGRAM uiop:run-program returns 3 values.
  (let ((exit-code (nth-value 2 (uiop:run-program "sbcl --load test-newenv.lisp --disable-debugger --quit"))))
    (is = 0 exit-code)))

