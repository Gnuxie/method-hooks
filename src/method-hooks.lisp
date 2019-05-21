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
a define-hook-generic form."
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

(defmacro %define-method-dispatch (generic-function qualifier specialized-lambda-list &body body)
  "defines the dispatch method for hooks, will remember the qualifier for the gf"
  (destructure-specialized-lambda-list descriptive-lambda-list vanilla-lambda-list type-list specialized-lambda-list
    (with-effective-qualifier generic-function qualifier
      `(%lay-method-base-for-dispatch ,generic-function ,qualifier ,type-list ,descriptive-lambda-list
         (dispatch ,generic-function ,qualifier ,specialized-lambda-list)
         ,@body))))

(defmacro %load-specializers-to-table (generic-function type-list qualifier)
  "creates a form to load the hooks specific to the gf/type-list/qualifier
from the compilation environment into the internal table inside the runtime environment."
  (let ((hooks (gensym)))
    `(let ((,hooks ',(mapcar #'hook-name (specific-hooks-for-generic type-list generic-function qualifier))))
       (mapc (lambda (f) (intern-hook ',generic-function f ',type-list ',qualifier))
             ,hooks))))

(defmacro defhook (generic-function hook-name &rest args)
  "define a hook to be to be called by the effective method.

This macro has roughly the same signature as defmethod `(DEFHOOK GENERIC-FUNCTION HOOK-NAME {QUALIFIER} SPECIALIZED-LAMBDA-LIST &BODY BODY)`
creates a function `hook-name` with the `body` then creates a method to dispatch all hooks matching
the type-list for the given generic-function.

See define-hook-generic
See finalize-dispatch-method"
  (%destructure-defhook-args args
    (destructure-specialized-lambda-list descriptive-lambda-list vanilla-lambda-list type-list specialized-lambda-list
      (with-effective-qualifier generic-function qualifier
        (intern-hook generic-function hook-name type-list qualifier)
        `(progn
           (%defhook-fun ,hook-name ,vanilla-lambda-list ,qualifier
                         ,@body)
           (%define-method-dispatch ,generic-function ,qualifier ,specialized-lambda-list))))))

(defmacro define-hook-generic (name gf-lambda-list &rest options)
  "utility to help with gf's with method combination by remembering the combination type

by default the combination type becomes the default qualifier for any newly defined hooks
this can be overriden by not using this and using defgeneric or supplying the option :default-qualifier.

See defhook"
  (let* ((combination-option (find :method-combination options :key #'car :test #'eql))
         (combination-type
          (cond ((null combination-option) 'progn)
                ((null (cadr combination-option)) :unqualified)
                (t (cadr combination-option))))
         (default-qualifier
          (let ((d (cadr (find :default-qualifier options :key #'car :test #'eql))))
            (if (null d) combination-type d))))

    (intern-hook-generic name combination-type default-qualifier)
    `(progn (intern-hook-generic ',name ',combination-type ',default-qualifier)
            (defgeneric ,name ,gf-lambda-list
              ,(concatenate
                'list
                (delete-if (lambda (s) (or (eql s :method-combination)
                                           (eql s :default-qualifier)))
                           options :key #'car)
                `(:method-combination
                  ,combination-type))))))

(defmacro finalize-dispatch-method (generic-function &rest args)
  "add a body to the method which dispatched the hooks for the given specialized-lambda-list
useful if you wanted to use call-next-method
defining another hook for the same qualified specific method after use will require recompilation
of the form as defhook will redefine the method.

See defhook"
  (%destructure-defhook-args args
    (with-effective-qualifier generic-function qualifier
      (destructure-specialized-lambda-list descriptive-lambda-list vanilla-lambda-list type-list specialized-lambda-list 
        `(%lay-method-base-for-dispatch ,generic-function ,qualifier ,type-list ,descriptive-lambda-list
           ,@body)))))


