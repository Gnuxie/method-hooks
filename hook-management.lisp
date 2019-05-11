#| this file is a part of method-hooks
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com> |#

(in-package :method-hooks)

(defvar *hook-functions* (make-hash-table)
  "hash table holding information on all of the generics used either indirectly with defhook or directly with define-hook-function")
(defvar *hooks* (make-hash-table) "information about all the hooks used in the image")

(defclass hook-function ()
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

(defclass hook ()
  ((qualifier :initarg :qualifier
              :accessor qualifier
              :type symbol)

   (name :initarg :name
         :accessor name
         :type symbol)))

(defun intern-undeclared-hook-function (gf-name)
  "if we stumble across a generic which we don't know about (ie from using defhook without define-hook-function)
then we must intern it in an appropriate way."
  (setf (gethash gf-name *hook-functions*)
        (make-instance 'hook-function
                       :combination :unqualified
                       :default-qualifier :unqualified)))

(defun intern-hook-function (gf-name method-combination default-qualifier)
  "will copy methods across if an existing gf is defined with them"
  (let ((new-hook-fun (make-instance 'hook-function
                                     :combination method-combination
                                     :default-qualifier default-qualifier))
        (existing-entry (gethash gf-name *hook-functions*)))
    
    (when existing-entry
      (setf (methods new-hook-fun)
            (methods existing-entry)))

    (setf (gethash gf-name *hook-functions*)
          new-hook-fun)))

(defun intern-hook (gf-name hook-name type-list qualifier)
  "intern the hook into the hooks hashtable by symbol name and into the generic functions table
by type-list and qualifier

as we are keeping references to hook objects in two places and due to the dificulty of keeping both
exactly up to date, specific-hooks-for-generic  will remove old references from the hook functions method list before returning the result."
  (when (null (gethash gf-name *hook-functions*))
    (intern-undeclared-hook-function gf-name))

  (let ((existing-hook (gethash hook-name *hooks*))
        (generic-function (gethash gf-name *hook-functions*)))
    (when (or (null existing-hook)
              (not (eql qualifier (qualifier existing-hook))))

      (if (null existing-hook)
          (setf existing-hook (make-instance 'hook :qualifier qualifier :name hook-name))
          (setf (qualifier existing-hook)
                qualifier))
      
      (let ((hooks (assoc qualifier (gethash type-list (methods generic-function)) :test 'eql)))

        ;; it's essential that the name is tested due to these forms being compiled and loaded in another env.
        (if (null hooks)
            (push (list qualifier existing-hook) (gethash type-list (methods generic-function)))
            (pushnew existing-hook (cdr (assoc qualifier (gethash type-list (methods generic-function)) :test 'eql)) :test #'eql :key #'name))

        (setf (gethash hook-name *hooks*)
              existing-hook)))))

(defun specific-hooks-for-generic (type-list generic-function qualifier)
  "get the hooks specific to the type specializer list and qualifier"
  (let ((qualified-list (gethash type-list (methods (gethash generic-function *hook-functions*)))))
    (symbol-macrolet ((hooks (cdr (assoc qualifier qualified-list))))
      (setf hooks
            (delete-if-not (lambda (h) (eql qualifier (qualifier h))) hooks)))))

(defun get-default-qualifier (gf-name)
  ;; this can be called before a hook gets interned due to `with-effective-qualifier`
  (macrolet ((gf-info ()`(gethash gf-name *hook-functions*)))
    (when (null (gf-info))
      (intern-undeclared-hook-function gf-name))
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