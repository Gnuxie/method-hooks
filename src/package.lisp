(defpackage #:method-hooks
  (:use #:cl)
  (:export
   #:defhook
   #:clear-hook-table
   #:finalize-dispatch-method
   #:define-hook-generic
   #:specific-hooks-for-generic

   
   #:dispatch-for-no-result
   #:dispatch-for-+

   #:set-dispatch-for-qualifier
   #:dispatch-for-qualifier
   #:define-dispatch
   #:dispatch
   #:make-dispatcher
   #:dispatcher
   #:dispatch-function-constructor
   #:dispatch-function

   #:hook-name
   #:qualifier
   #:hook


))
