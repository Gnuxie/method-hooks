(in-package :method-hooks-test)

(defun run (&key (report 'plain))
  (test 'method-hooks-test :report report))

(defun ci-run ()
  (let ((test-result (run)))
    (when (not (null (results-with-status :failed test-result)))
      (uiop:quit -1))))
