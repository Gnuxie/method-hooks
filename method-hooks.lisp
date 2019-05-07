#| this file is a part of method-hooks
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com> |#

(in-package :method-hooks)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *hooks* (make-hash-table))
  (defvar *hf-default-qualifiers* (make-hash-table)))

(defun %produce-specific-hooks (type-list generic-function qualifier)
  "returns a list of hooks specific to the type-list and qualifier for the generic function"
  (cdr (assoc qualifier (gethash type-list (gethash generic-function *hooks*)) :test 'eql)))

(defun %intern-hook (generic-function hook-name type-list &optional (qualifier :unqualified))
  "intern the hook into the internal hook table by symbol name.

The first hash table dimension  contains the generic function names.
The second dimension contains the type specializer lists for each method to that generic function.
The third dimension is an assoc list with qualifiers for the assoc keys and a list of all the hooks relevant to that method &
qualifier."
  (when (null (gethash generic-function *hooks*))
    (setf (gethash generic-function *hooks*) (make-hash-table :test 'equal)))

  (let ((hooks (assoc qualifier (gethash type-list (gethash generic-function *hooks*)) :test 'eql)))
    (if (null hooks)
        (push (list qualifier hook-name) (gethash type-list (gethash generic-function *hooks*)))
        (pushnew hook-name (cdr (assoc qualifier (gethash type-list (gethash generic-function *hooks*)) :test 'eql))))))

(defmacro %defhook-fun (hook-name vanilla-lambda-list qualifier &body body)
  "this is literally just a copy of defun atm."
  `(defun ,hook-name 
       ,vanilla-lambda-list
     ,@body))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-default-qualifier (gf-name)
    "fetch the default qualifier for the generic function."
    (gethash gf-name *hf-default-qualifiers*)))

(defmacro with-effective-qualifier (gf-name qualifier &body body)
  "take generic function and a symbol bound to a qualifier and mask that symbol with the effective qualifier.

The effective qualifier is the default qualifier for the given generic function should there be one defined by
a define-hook-function form."
  (let ((combination-type (get-default-qualifier gf-name)))
    `(let ((,qualifier
            ,(cond ((null combination-type) qualifier)             ; gf has no combination type, so unqualified is valid.
                   ((eql qualifier :unqualified) combination-type) ; gf has combination-type, use default from table
                   (t qualifier))))
       ,@body)))

(defmacro %define-method-dispatch (generic-function qualifier descriptive-lambda-list vanilla-lambda-list type-list &body body)
  "defines the dispatch method for hooks, will remember the qualifier for the gf"
  (with-effective-qualifier generic-function qualifier
    `(progn (%load-specializers-to-table ,generic-function ,type-list ,qualifier)
            ,(delete :unqualified
                     `(defmethod ,generic-function ,qualifier ,descriptive-lambda-list
                                 (mapc (lambda (f) (funcall f . ,vanilla-lambda-list))
                                       ',(%produce-specific-hooks type-list generic-function  qualifier))

                                 ,@body)
                                             :end (if (eql :unqualified qualifier) 3 0)))))

(defmacro destructure-lambda-list (descriptive-lambda-list-sym vanilla-lambda-list-sym type-list-sym lambda-list &body body)
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
  "creates a form to load the hooks specific to the gf/type-specializer-list/qualifier
from the compilation environment into the internal table inside the runtime environment."
  (let ((hooks (gensym)))
    `(let ((,hooks ',(%produce-specific-hooks type-list generic-function qualifier)))
       (mapc (lambda (f) (%intern-hook ',generic-function f ',type-list ',qualifier))
             ,hooks))))

(defmacro defhook (generic-function hook-name lambda-list (&optional (qualifier :unqualified)) &body body)
  "define a hook to be to be called by the effective method.

creates a function `hook-name` with the `body` then creates a method to dispatch all hooks matching
the type specializer list for the given generic-function."
  (destructure-lambda-list descriptive-lambda-list vanilla-lambda-list type-list lambda-list
    (with-effective-qualifier generic-function qualifier
      (%intern-hook generic-function hook-name type-list qualifier)

        `(progn
           (%defhook-fun ,hook-name ,vanilla-lambda-list ,qualifier
                         ,@body)
           (%define-method-dispatch ,generic-function ,qualifier ,descriptive-lambda-list ,vanilla-lambda-list ,type-list)))))

(defmacro define-hook-function (name gf-lambda-list &args options)
  "utility to help with gf's with method combination by remembering the combination type

by default the combination type becomes the default qualifier for new hooks
this can be overriden by not using this and using defgeneric. 
I might add a way to override it from here too."
  (macrolet ((%intern-gf-combination (gf-name combination-type)
               `(setf (gethash ,gf-name *hf-default-qualifiers*)
                      (if (null combination-type) 'progn combination-type))))
    (let ((combination-type (cdr (find :method-combination options :key #'car :test #'eql))))
      (%intern-gf-combination name combination-type)
      `(progn (%intern-gf-combination ,name ,combination-type)
              (defgeneric ,name ,gf-lambda-list
                ,(concatenate
                  'list
                  (delete :method-combination options :key #'car :test #:eql)
                  '((:method-combination
                     (if (null combination-type) 'progn combination-type)))))))))

(defmacro finalize-dispatch-method (generic-function lambda-list (&optional (qualifier :unqualified)) &body body)
  "add a body to the method which dispatched the hooks for the given type specializer list
useful if you wanted to use call-next-method
defining another hook for the same gf and type specializer list after use will require recompilation
of the form. "
   (destructure-lambda-list descriptive-lambda-list vanilla-lambda-list type-list lambda-list 
     `(%define-method-dispatch ,generic-function ,qualifier ,descriptive-lambda-list ,vanilla-lambda-list ,type-list
                               ,body)))

(defun clear-hook-table ()
  "will not require recompilation of all the forms unless an existing hook definition is redefined
or a method is redefined"
  (setf *hooks* (make-hash-table)))
