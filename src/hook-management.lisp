#| this file is a part of method-hooks
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com> |#

(in-package :method-hooks)

(defvar *hook-generics* (make-hash-table)
  "hash table holding information on all of the generics used either indirectly with defhook or directly with define-hook-generic")
(defvar *hooks* (make-hash-table) "information about all the hooks used in the image")

(defclass hook-generic ()
  ((methods :initarg :methods
            :accessor methods
            :initform (make-hash-table :test #'equal)
            :type hash-table)

   (combination :initarg :combination
                :accessor combination
                :type symbol)

   (default-qualifier :initarg :default-qualifier
                        :accessor default-qualifier
                        :type symbol)))
#|
(defmethod make-load-form ((self hook-generic) &optional environment)
  (declare (ignore environment))
  `(make-instance 'hook-generic ; we don't want to put the hash table in atm bc we have %load-specializers-to...
                  :combination ',(combination self)
                  :default-qualifier ',(default-qualifier self)))
|#
(defclass hook ()
  ((qualifier :initarg :qualifier
              :accessor qualifier
              :type symbol)

   (hook-name :initarg :hook-name
         :accessor hook-name
         :type symbol)))

#|
(defmethod make-load-form ((self hook) &optional environment)
  `(make-instance 'hook :hook-name ',(hook-name self) :qualifier ',(qualifier self)))
|#
(defun intern-undeclared-hook-generic (gf-name)
  "if we stumble across a generic which we don't know about (ie from using defhook without define-hook-generic)
then we must intern it in an appropriate way."
  (setf (gethash gf-name *hook-generics*)
        (make-instance 'hook-generic
                       :combination :unqualified
                       :default-qualifier :unqualified)))

(defun intern-hook-generic (gf-name method-combination default-qualifier)
  "will copy methods across if an existing gf is defined with them"
  (let ((new-hook-generic (make-instance 'hook-generic
                                     :combination method-combination
                                     :default-qualifier default-qualifier))
        (existing-entry (gethash gf-name *hook-generics*)))

    (when existing-entry
      (setf (methods new-hook-generic)
            (methods existing-entry)))

    (setf (gethash gf-name *hook-generics*)
          new-hook-generic)))

(defun intern-hook (gf-name hook-name type-list qualifier)
  "intern the hook into the hooks hashtable by symbol name and into the generic functions table
by type-list and qualifier

as we are keeping references to hook objects in two places and due to the dificulty of keeping both
exactly up to date, specific-hooks-for-generic  will remove old references from the hook functions method list before returning the result."
  (when (null (gethash gf-name *hook-generics*))
    (intern-undeclared-hook-generic gf-name))

  (let ((existing-hook (gethash hook-name *hooks*))
        (generic-function (gethash gf-name *hook-generics*)))
    (when (or (null existing-hook)
              (not (eql qualifier (qualifier existing-hook))))

      (if (null existing-hook)
          (setf existing-hook (make-instance 'hook :qualifier qualifier :hook-name hook-name))
          (setf (qualifier existing-hook)
                qualifier))

      (let ((hooks (assoc qualifier (gethash type-list (methods generic-function)) :test 'eql)))

        ;; it's essential that the name is tested due to these forms being compiled and loaded in another env.
        (if (null hooks)
            (push (list qualifier existing-hook) (gethash type-list (methods generic-function)))
            (pushnew existing-hook (cdr (assoc qualifier (gethash type-list (methods generic-function)) :test 'eql)) :test #'eql :key #'hook-name))

        (setf (gethash hook-name *hooks*)
              existing-hook)))))

(defun specific-hooks-for-generic (type-list generic-function qualifier)
  "get the hooks specific to the type specializer list and qualifier"
  (let ((qualified-list (gethash type-list (methods (gethash generic-function *hook-generics*)))))
    (symbol-macrolet ((hooks (cdr (assoc qualifier qualified-list))))
      (unless (null hooks)
        (setf hooks
              (remove-if-not (lambda (h) (eql qualifier (qualifier h))) hooks))))))

(defun get-default-qualifier (gf-name)
  ;; this can be called before a hook gets interned due to `with-effective-qualifier`
  (macrolet ((gf-info ()`(gethash gf-name *hook-generics*)))
    (when (null (gf-info))
      (intern-undeclared-hook-generic gf-name))
    (default-qualifier (gf-info))))

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
