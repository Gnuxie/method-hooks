(defpackage #:method-hooks-package-test
  (:use #:cl #:method-hooks)
  (:export
   #:reset-tables))

(in-package :method-hooks-package-test)

(defun reset-tables ()
  (clear-hook-table))

(defpackage #:method-hooks-test
  (:use #:cl #:method-hooks #:parachute)
  (:export
   #:run
   #:ci-run))

(in-package :method-hooks-test)
(clear-hook-table)

(defvar *result* 1)

(defgeneric accumulating (a)
  (:method-combination progn))

(method-hooks:defhook accumulating add-once ((x integer)) (progn)
  (incf *result* x))

(defhook accumulating add-twice ((x integer)) (progn)
  (incf *result* x))

(defhook accumulating add-thrice ((x integer)) (progn)
  (incf *result* x))

(defvar *qualifier-before-test* nil)
(defvar *qualifier-test-pass* nil)
(defgeneric qualifier-test (a))

(defhook qualifier-test not-qualified ((a integer)) ()
    (when *qualifier-before-test*
      (setf *qualifier-test-pass* t)))

(defhook qualifier-test before-qualified ((a integer)) (:before)
    (setf *qualifier-before-test* t))

(define-test method-hooks-test

  (setf *qualifier-before-test* nil)
  (setf *qualifier-test-pass* nil)

  (define-test spooky-package-test

    ;; clearing the tables actually doesn't do anything when the methods have been compiled
    ;; as any existing functions will not be effected by the tables being cleared.
    ;; when any hook is recompiled the tables will be repopulated anyways.
    ;; or this is how it should stay
    (method-hooks-package-test:reset-tables)
    ;; not sure what can be directly tested here.
    (true (symbol-function 'before-qualified)))
  
  (define-test serial-test
    :depends-on (spooky-package-test)

    (setf *result* 0)
    (accumulating 2)
    (is = 6 *result*))

  (define-test redifinition-test
    :depends-on (serial-test) ; "depends on", really we need to ensure this happens afterwards.
    (setf *result* 0)

    (setf (symbol-function 'add-twice)
          (lambda (x) (incf *result* (* 2 x)))
          (symbol-function 'add-thrice)
          (lambda (x) (incf *result* (* 3 x))))

    (accumulating 2)
    (is = 12 *result*))

  (define-test qualified-method-test
    :depends-on (spooky-package-test)
    
    (qualifier-test 3)
    (true *qualifier-test-pass*)))

(defun run (&key (report 'plain))
  (test 'method-hooks-test :report report))

(defun ci-run ()
  (let ((test-result (run)))
    (when (not (null (results-with-status :failed test-result)))
      (uiop:quit -1))))
