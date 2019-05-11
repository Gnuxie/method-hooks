(defpackage #:method-hooks
  (:use #:cl)
  (:export
   #:defhook
   #:clear-hook-table
   #:finalize-dispatch-method
   #:define-hook-function
   #:specific-hooks-for-generic)
  )


