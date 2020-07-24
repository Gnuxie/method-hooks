#| this file is a part of method-hooks
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com> |#

(in-package :method-hooks)

(defvar *dispatch-table* (make-hash-table)
  "used to lookup the dispatcher for a symbol")
(defvar *dispatch-for-qualifier* (make-hash-table)
  "used to lookup the symbol for a dispatcher")

(defun dispatch-for-qualifier (qualifier)
  "the dispatcher used for the given qualifier

See define-dispatch
See dispatcher"
  (gethash (gethash qualifier *dispatch-for-qualifier*)
           *dispatch-table*))

(defun set-dispatch-for-qualifier (qualifier dispatch)
  "accepts two symbols, sets the dispatcher to be used for the given qualifier

See define-dispatch
See dispatcher"
  (setf (gethash qualifier *dispatch-for-qualifier*)
         dispatch))

(defclass dispatcher ()
  ((dispatch-function-constructor :accessor dispatch-function-constructor
                         :initarg :dispatch-function-constructor
                         :type list
                         :documentation "a lambda expression used to construct the function object for dispatch-function.")

   (dispatch-function :accessor dispatch-function
                   :initarg :dispatch-function
                   :type function
                   :documentation "a function object for the current environment")))

(defmacro make-dispatcher (function-constructor)
  `(make-instance 'dispatcher :dispatch-function-constructor ',function-constructor
                  :dispatch-function ,function-constructor))

(defmacro define-dispatch (name lambda-list &body body)
  "the lambda list should accept two arguments:
the list of arguments given by the current method call.
the specific hooks (as named or unamed functions) for the qualified method (that we will be dispatching from).

you should then use the arguments to dispatch the specific hooks as you wish in the body.
if the body returns a result, by default the method will also return that result, this can be overriden with finalize-dispatch-method.

See finalize-dispatch-method
See dispatch"
  `(let ((new-dispatch (make-dispatcher (lambda ,lambda-list ,@body))))
     (setf (gethash ',name *dispatch-table*)
           new-dispatch)))

(defmethod make-load-form ((self dispatcher) &optional environment)
  (declare (ignore environment))
  `(make-dispatcher ,(dispatch-function-constructor self)))

(defmacro dispatch (generic-function qualifier specialized-lambda-list)
  "dispatch the hooks using the default dispatcher for the given qualified specific method.

See define-dispatch
See dispatch-function
See set-dispatch-for-qualifier"
  (destructure-specialized-lambda-list descriptive-lambda-list vanilla-lambda-list type-list specialized-lambda-list
     `(funcall (dispatch-function (dispatch-for-qualifier ',qualifier))
               (list ,@vanilla-lambda-list)
               (mapcar #'hook-name
                       (specific-hooks-for-generic ',type-list ',generic-function ',qualifier)))))
