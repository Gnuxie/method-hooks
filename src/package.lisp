(defpackage #:method-hooks
  (:use #:cl)
  (:export
   #:defhook
   #:clear-hook-table
   #:finalize-dispatch-method
   #:define-hook-function
   #:specific-hooks-for-generic

   
   #:dispatch-for-no-result
   #:dispatch-for-+

   #:set-dispatch-for-qualifier
   #:dispatch-for-qualifier
   #:define-dispatch
   #:dispatch

))
