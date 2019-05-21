(asdf:defsystem "method-hooks"
  :version "0.1.0"
  :author "Gnuxie <Gnuxie@protonmail.com>, theemacsshibe"
  :maintainer "Gnuxie <Gnuxie@protonmail.com>"
  :licence "Mozilla Public License Version 2.0"
  :homepage "https://gnuxie.gitlab.io/method-hooks/"
  :serial t
  :components  ((:module "src" :components
                         ((:file "package")
                          (:file "macro-utils")
                          (:file "combination-management")
                          (:file "known-dispatchers")
                          (:file "hook-management")
                          (:file "method-hooks"))))
  :description "simple method hooks defined like methods and dispatched via method")
