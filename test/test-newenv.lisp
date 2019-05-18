(ql:register-local-projects)
(asdf:load-system :method-hooks-test)

(loop :while (null (find-package "METHOD-HOOKS-TEST")) :do
     (sleep 1))

;;; we redefine this method bc we need to see if the METHOD-HOOKS::*hooks* table has been
;;; replenished on load. if it hasn't, the method dispatcher that is defined here will only dispatch
;;; `add-thrice` and not any of the other hooks defined in the other file.
(in-package :method-hooks-test)
(method-hooks:defhook accumulating add-thrice progn ((x integer))
  (incf *result* x))

(method-hooks:defhook addtest addonce ((x integer))
  x)

(let ((result (parachute:test 'method-hooks-test::comprehensive-test)))
  (if (null (results-with-status :failed result))
      (uiop:quit)
      (uiop:quit -1)))

