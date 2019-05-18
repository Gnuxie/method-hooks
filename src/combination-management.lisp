#| this file is a part of method-hooks
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com> |#

(in-package :method-hooks)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *dispatch-table* (make-hash-table)
    "used to lookup the dispatcher for a symbol")
  (defvar *dispatch-for-qualifier* (make-hash-table)
    "used to lookup the symbol for a dispatcher"))

(defun dispatch-for-qualifier (qualifier)
  (gethash (gethash qualifier *dispatch-for-qualifier*)
           *dispatch-table*))

(defun set-dispatch-for-qualifier (qualifier dispatch)
  "accepts two symbols."
  (setf (gethash qualifier *dispatch-for-qualifier*)
         dispatch))

(defclass dispatcher ()
  ((function-constructor :accessor function-constructor
                         :initarg :function-constructor
                         :type list
                         :documentation "a lambda expression used to construct the function object for function-value.")

   (function-value :accessor function-value
                   :initarg :function-value
                   :type function
                   :documentation "a function object for the current environment")))

(defmacro make-dispatcher (function-constructor)
  `(make-instance 'dispatcher :function-constructor ',function-constructor
                  :function-value ,function-constructor))

(defmacro define-dispatch (name lambda-list &body body)
  "the lambda list should accept two arguments:
the list of arguments given by the current method call.
the the specific hooks (as named or unamed functions) for the qualified method (that we will be dispatching from)."
  `(let ((new-dispatch (make-dispatcher (lambda ,lambda-list ,@body))))
     (setf (gethash ',name *dispatch-table*)
           new-dispatch)))

(defmethod make-load-form ((self dispatcher) &optional environment)
  (declare (ignore environment))
  `(make-dispatcher ,(function-constructor self)))