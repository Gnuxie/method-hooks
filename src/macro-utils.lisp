#| this file is a part of method-hooks
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com> |#

(in-package :method-hooks)

(defmacro %destructure-defhook-args (args &body body)
  (let ((args-sym (gensym)))
    `(let* ((,args-sym ,args)
            (qualifier
            (cond ((and (symbolp (car ,args-sym)) (not (null (car ,args-sym))))
                   (prog1 (car ,args-sym) (setf ,args-sym (cdr ,args-sym))))
                  (t :use-default))))
       (destructuring-bind (lambda-list &body body) ,args-sym
         ,@body))))


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
