(asdf:defsystem "method-hooks"
  :version "0.1.0"
  :author "Gnuxie <Gnuxie@protonmail.com>, theemacsshibe"
  :maintainer "Gnuxie <Gnuxie@protonmail.com>"
  :licence "Mozilla Public License Version 2.0"
  :homepage "https://gnuxie.gitlab.io/method-hooks/"
  :bug-tracker "https://gitlab.com/Gnuxie/method-hooks/issues"
  :serial t
  :components  ((:module "src" :components
                         ((:file "package")
                          (:file "macro-utils")
                          (:file "combination-management")
                          (:file "known-dispatchers")
                          (:file "hook-management")
                          (:file "method-hooks"))))
  :description "simple qualifiable hooks defined like methods with the option to modify the dispatch method and how dispatch happens")
