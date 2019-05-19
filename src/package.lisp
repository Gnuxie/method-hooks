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

))
