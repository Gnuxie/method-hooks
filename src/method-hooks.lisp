#| this file is a part of method-hooks
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com> |#

(in-package :method-hooks)

(defmacro %defhook-fun (hook-name vanilla-lambda-list qualifier &body body)
  "this is literally just a copy of defun atm."
  `(defun ,hook-name 
       ,vanilla-lambda-list
     ,@body))

(defmacro with-effective-qualifier (gf-name qualifier &body body)
  "take generic function and a symbol bound to a qualifier and mask that symbol with the effective qualifier.

The effective qualifier is the default qualifier for the given generic function should there be one defined by
a define-hook-function form."
  `(let ((,qualifier
          (effective-qualifier ,gf-name ,qualifier)))
     ,@body))

(defmacro %lay-method-base-for-dispatch (generic-function qualifier type-list descriptive-lambda-list &body body)
  (with-effective-qualifier generic-function qualifier
    `(progn (%load-specializers-to-table ,generic-function ,type-list ,qualifier)
       ,(delete :unqualified
                     `(defmethod ,generic-function ,qualifier ,descriptive-lambda-list
                                 ,@body)
                     :end (if (eql :unqualified qualifier) 3 0)))))

(defmacro %define-method-dispatch (generic-function qualifier type-specializer-list &body body)
  "defines the dispatch method for hooks, will remember the qualifier for the gf"
  (destructure-lambda-list descriptive-lambda-list vanilla-lambda-list type-list type-specializer-list
    (with-effective-qualifier generic-function qualifier
      `(%lay-method-base-for-dispatch ,generic-function ,qualifier ,type-list ,descriptive-lambda-list
         (dispatch ,generic-function ,qualifier ,type-specializer-list)
         ,@body))))

(defmacro %load-specializers-to-table (generic-function type-list qualifier)
  "creates a form to load the hooks specific to the gf/type-specializer-list/qualifier
from the compilation environment into the internal table inside the runtime environment."
  (let ((hooks (gensym)))
    `(let ((,hooks ',(mapcar #'name (specific-hooks-for-generic type-list generic-function qualifier))))
       (mapc (lambda (f) (intern-hook ',generic-function f ',type-list ',qualifier))
             ,hooks))))

(defmacro defhook (generic-function hook-name &rest args); {qualifier} lambda-list &body body
  "define a hook to be to be called by the effective method.

creates a function `hook-name` with the `body` then creates a method to dispatch all hooks matching
the type specializer list for the given generic-function."
  (%destructure-defhook-args args
    (destructure-lambda-list descriptive-lambda-list vanilla-lambda-list type-list lambda-list
      (with-effective-qualifier generic-function qualifier
        (intern-hook generic-function hook-name type-list qualifier)
        `(progn
           (%defhook-fun ,hook-name ,vanilla-lambda-list ,qualifier
                         ,@body)
           (%define-method-dispatch ,generic-function ,qualifier ,lambda-list))))))

(defmacro define-hook-function (name gf-lambda-list &rest options)
  "utility to help with gf's with method combination by remembering the combination type

by default the combination type becomes the default qualifier for new hooks
this can be overriden by not using this and using defgeneric. 
I might add a way to override it from here too."
  (let* ((combination-option (find :method-combination options :key #'car :test #'eql))
         (combination-type
          (cond ((null combination-option) 'progn)
                ((null (cadr combination-option)) :unqualified)
                (t (cadr combination-option)))))
  
    (intern-hook-function name combination-type combination-type)
    `(progn (intern-hook-function ',name ',combination-type ',combination-type)
            (defgeneric ,name ,gf-lambda-list
              ,(concatenate
                'list
                (delete :method-combination options :key #'car :test #'eql)
                `(:method-combination
                  ,combination-type))))))

(defmacro finalize-dispatch-method (generic-function &rest args)
  "add a body to the method which dispatched the hooks for the given type specializer list
useful if you wanted to use call-next-method
defining another hook for the same gf and type specializer list after use will require recompilation
of the form. "
  (%destructure-defhook-args args
    (with-effective-qualifier generic-function qualifier
      (destructure-lambda-list descriptive-lambda-list vanilla-lambda-list type-list lambda-list 
        `(%lay-method-base-for-dispatch ,generic-function ,qualifier ,type-list ,descriptive-lambda-list
           ,@body)))))


