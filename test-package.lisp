(defpackage #:method-hooks-package-test
  (:use #:cl #:method-hooks)
  (:export
   #:reset-tables))

(defpackage #:method-hooks-test
  (:use #:cl #:method-hooks #:parachute)
  (:export
   #:run
   #:ci-run
   #:method-hooks-test
   #:comprehensive-test))

(defpackage #:method-hooks-environment-test
  (:use #:cl #:parachute #:method-hooks)
  (:export #:environment-test))
