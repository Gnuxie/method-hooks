(in-package :method-hooks-test)

(defun run (&key (report 'plain))
  (test 'method-hooks-test :report report))

(defun ci-run ()
  (let ((test-result nil))
    (unwind-protect (setf test-result (run))
      (cond ((null test-result)
             (format t "~%error running tests~%") (uiop:quit -1))
            ((null (results-with-status :failed test-result))
             (uiop:quit 0))
            (t (uiop:quit -1))))))
