(asdf:defsystem "method-hooks"
  :version "0.1.0"
  :author "Gnuxie <Gnuxie@protonmail.com>, theemacsshibe"
  :maintainer "Gnuxie <Gnuxie@protonmail.com>"
  :licence "Mozilla Public License Version 2.0"
  :serial t
  :components ((:file "package")
               (:file "hook-management")
               (:file "method-hooks"))
  :description "simple method hooks defined like methods and dispatched via method")
