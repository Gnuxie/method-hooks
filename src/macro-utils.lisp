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
       (destructuring-bind (specialized-lambda-list &body body) ,args-sym
         ,@body))))


(defmacro destructure-specialized-lambda-list (descriptive-lambda-list-sym vanilla-lambda-list-sym type-list-sym specialized-lambda-list &body body)
  "if specialized-lambda-list is ((a integer) b):
descriptive-lambda-list will be ((x integer) (x t))
type-list will be (integer t) and vanilla-lambda-list is (a b)"
  (let ((item (gensym)))
    `(let* ((,descriptive-lambda-list-sym
             (loop :for ,item :in ,specialized-lambda-list
                :if (listp ,item)
                :collect ,item
                :else :collect (list ,item t)))
            (,vanilla-lambda-list-sym (mapcar #'first ,descriptive-lambda-list-sym))
            (,type-list-sym (mapcar #'second ,descriptive-lambda-list-sym)))
       (declare (ignorable ,descriptive-lambda-list-sym ,vanilla-lambda-list-sym ,type-list-sym))
       ,@body)))
