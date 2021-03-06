(in-package :method-hooks-package-test)

(defun reset-tables ()
  (clear-hook-table))

(in-package :method-hooks-test)

(define-test method-hooks-test)

(clear-hook-table)

(defvar *result* 1)

(defgeneric accumulating (a)
  (:method-combination progn))
(define-hook-generic addtest (x)
  (:method-combination +))
(define-hook-generic progntest (a))
(define-hook-generic hook-point-generic ()
  (:documentation "because no one tested this last time.")
  (:method-combination +)
  (:hook-point t))

(define-hook-generic bad-hook-point ())

(method-hooks:defhook accumulating add-once progn ((x integer))
  (incf *result* x))

(defhook accumulating add-twice progn ((x integer))
  (incf *result* x))

(defhook accumulating add-thrice progn ((x integer)) 
  (incf *result* x))

(defvar *qualifier-before-test* nil)
(defvar *qualifier-test-pass* nil)
;;; these two are required by spooky-package-test and qualified-method-test
(defgeneric qualifier-test (a))
(defhook qualifier-test not-qualified ((a integer))
  (when *qualifier-before-test*
    (setf *qualifier-test-pass* t)))
(defhook qualifier-test before-qualified :before ((a integer))
  (setf *qualifier-before-test* t))

(define-test comprehensive-test
  :parent method-hooks-test
  
  (setf *qualifier-before-test* nil)
  (setf *qualifier-test-pass* nil)

  (define-test spooky-package-test

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

    (with-fixtures '(add-twice add-thrice)
      (setf *result* 0)

      (setf (symbol-function 'add-twice)
            (lambda (x) (incf *result* (* 2 x)))
            (symbol-function 'add-thrice)
            (lambda (x) (incf *result* (* 3 x))))

      (accumulating 2)
      (is = 12 *result*)))

  (define-test qualified-method-test
    :depends-on (spooky-package-test)

    (with-fixtures '(*qualifier-test-pass*)
      (qualifier-test 3)
      (true *qualifier-test-pass*))

    (let ((count 0))
      (with-fixtures '(count)
        (defhook change-the-qualifier changing-this ()
                 (incf count))

        (defhook change-the-qualifier changing-this :before ()
                 (incf count))

        (change-the-qualifier)
        (is = 1 count))))

  (define-test qualified-hook-generic
    :depends-on (qualified-method-test)

    (let ((sum 0))
      (defhook progntest prognonce ((a integer))
        (incf sum))

      (defhook progntest progntwice ((a integer))
        (incf sum))
      
      (define-test check-effective-qualifier

        (let ((qualifier :use-default))
          (method-hooks::with-effective-qualifier progntest qualifier
            (is eql 'progn qualifier))))

      (progntest 0)
      (is = 2 sum))

    (defhook addtest addonce ((x integer))
      x)

    (defhook addtest addtwice ((x integer))
      x)

    (is = 4 (addtest 2)))

  (define-test hook-point-test
      :depends-on (qualified-method-test)

      (true (progn (hook-point-generic) t)
            "no applicable method error doesn't singal")

      (fail (bad-hook-point))))
