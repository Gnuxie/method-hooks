#| this file is a part of method-hooks
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com> |#

(in-package :method-hooks)

(defvar *hooks* (make-hash-table))

(defun %produce-specific-hooks (type-list generic-function qualifier)
  (cdr (assoc qualifier (gethash type-list (gethash generic-function *hooks*)) :test 'eql)))

(defun %intern-hook (generic-function hook-name type-list &optional (qualifier :unqualified))
  (when (null (gethash generic-function *hooks*))
    (setf (gethash generic-function *hooks*) (make-hash-table :test 'equal)))

  (let ((hooks (assoc qualifier (gethash type-list (gethash generic-function *hooks*)) :test 'eql)))
    (if (null hooks)
        (push (list qualifier hook-name) (gethash type-list (gethash generic-function *hooks*)))
        (pushnew hook-name (cdr (assoc qualifier (gethash type-list (gethash generic-function *hooks*)) :test 'eql))))))

(defmacro define-hook-function (hook-name vanilla-lambda-list qualifier &body body)
  `(defun ,hook-name 
       ,vanilla-lambda-list
     ,@body))

(defmacro %define-method-dispatch (generic-function qualifier descriptive-lambda-list vanilla-lambda-list type-list &body body)
  (delete :unqualified
          `(defmethod ,generic-function ,qualifier ,descriptive-lambda-list
                      (mapc (lambda (f) (funcall f . ,vanilla-lambda-list))
                            ',(%produce-specific-hooks type-list generic-function  qualifier))

                      ,@body)
          :end (if (eql :unqualified qualifier) 3 0)))

(defmacro %destructure-lambda-list (descriptive-lambda-list-sym vanilla-lambda-list-sym type-list-sym lambda-list &body body)
  "if lambda-list=((a integer) b), type-list is (integer t) and vanilla-lambda-list is (a b)"
  (let ((item (gensym)))
    `(let* ((,descriptive-lambda-list-sym
             (loop :for ,item :in ,lambda-list
                :if (listp ,item)
                :collect ,item
                :else :collect (list ,item t)))
            (,vanilla-lambda-list-sym (mapcar #'first ,descriptive-lambda-list-sym))
            (,type-list-sym (mapcar #'second ,descriptive-lambda-list-sym)))
       ,@body)))

(defmacro %load-specializers-to-table (generic-function type-list qualifier)
  (let ((hooks (gensym)))
    `(let ((,hooks ',(%produce-specific-hooks type-list generic-function qualifier)))
       (mapc (lambda (f) (%intern-hook ',generic-function f ',type-list ',qualifier))
             ,hooks))))

(defmacro defhook (generic-function hook-name lambda-list (&optional (qualifier :unqualified)) &body body)
  "define a hook to be to be called by the effective method.

creates a function `hook-name` with the `body` then creates a method to dispatch all hooks matching
the type specializer list for the given generic-function."
  (%destructure-lambda-list descriptive-lambda-list vanilla-lambda-list type-list lambda-list
    (%intern-hook generic-function hook-name type-list qualifier)

    `(progn
       (%load-specializers-to-table ,generic-function ,type-list ,qualifier)
       (define-hook-function ,hook-name ,vanilla-lambda-list ,qualifier
                             ,@body)

       (%define-method-dispatch ,generic-function ,qualifier ,descriptive-lambda-list ,vanilla-lambda-list ,type-list))))

(defmacro finalize-dispatch-method (generic-function lambda-list (&optional (qualifier :unqualified)) &body body)
  "add a body to the method which dispatched the hooks for the given type specializer list
useful if you wanted to use call-next-method
defining another hook for the same gf and type specializer list after use will require recompilation
of the form. "
   (%destructure-lambda-list descriptive-lambda-list vanilla-lambda-list type-list lambda-list 
     `(%define-method-dispatch ,generic-function ,qualifier ,descriptive-lambda-list ,vanilla-lambda-list ,type-list
                               ,body)))

(defun clear-hook-table ()
  "will not require recompilation of all the forms unless an existing hook definition is redefined
or a method is redefined"
  (setf *hooks* (make-hash-table)))
