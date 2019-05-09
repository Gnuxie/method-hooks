#| this file is a part of method-hooks
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com> |#

(in-package :method-hooks)

(defvar *hooks* (make-hash-table))
(defvar *hf-default-qualifiers* (make-hash-table))

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

(defun get-default-qualifier (gf-name)
  "fetch the default qualifier for the generic function."
  (gethash gf-name *hf-default-qualifiers*))

(defun effective-qualifier (gf-name qualifier)
  (let ((combination-type (get-default-qualifier gf-name)))
    (cond ((and (null combination-type) (eql qualifier :use-default))
           :unqualified)
          ((eql qualifier :use-default) combination-type)
          (t qualifier))))

(defun clear-hook-table ()
  "will not require recompilation of all the forms unless an existing hook definition is redefined
or a method is redefined"
  (setf *hooks* (make-hash-table)))
