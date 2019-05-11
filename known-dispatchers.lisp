#| this file is a part of method-hooks
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com> |#

(in-package :method-hooks)

(define-dispatch dispatch-for-no-result (args specific-hooks)
  (mapc (lambda (f) (apply f args))
        specific-hooks))

(define-dispatch dispatch-for-+ (args specific-hooks)
  (reduce #'+ (mapcar (lambda (f) (apply f args))
                      specific-hooks)))

(dolist (qualifier '(:unqualified :before :around :after progn))
  (set-dispatch-for-qualifier qualifier 'dispatch-for-no-result))

(set-dispatch-for-qualifier '+ 'dispatch-for-+)
