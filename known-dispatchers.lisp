#| this file is a part of method-hooks
   Copyright (C) 2019 Gnuxie <Gnuxie@protonmail.com> |#

(in-package :method-hooks)

(set-dispatch-for-qualifiers (:unqualified :before :around :after progn)
  (lambda (args specific-hooks)
    (mapc (lambda (f) (apply f args))
            specific-hooks)))

(set-dispatch-for-qualifier +
  (lambda (args specific-hooks)
    (reduce #'+ (mapcar (lambda (f) (apply f args))
                        specific-hooks))))
