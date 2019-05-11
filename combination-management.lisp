#| this file is a part of method-hooks
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com> |#

(in-package :method-hooks)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *dispatch-for-qualifier* (make-hash-table)))

(defun dispatch-for-qualifier (qualifier)
  (gethash qualifier *dispatch-for-qualifier*))

(defmacro set-dispatch-for-qualifier (qualifier dispatch-function)
  "a dispatcher must be a function which accepts two arguments:
the list of arguments given by the current method call.
the the specific hooks (as named or unamed functions) for the qualified method (that we will be dispatching from)."
  `(setf (gethash ',qualifier *dispatch-for-qualifier*)
         ',dispatch-function))

(defmacro set-dispatch-for-qualifiers ((&rest qualifiers) dispatch-function)
  `(progn
     ,@ (loop :for qualifier :in qualifiers :collect
             `(set-dispatch-for-qualifier ,qualifier ,dispatch-function))))

(defmacro %load-dispatchers ()
  (let ((continue? t))
    (with-hash-table-iterator (generator-fn *dispatch-for-qualifier*)
      `(progn ,@
         (loop :while continue? :collect
              (multiple-value-bind (more? key value) (generator-fn)
                (setf continue? more?)

                `(set-dispatch-for-qualifier ,key ,value)))))))
