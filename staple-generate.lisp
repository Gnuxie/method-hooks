(ql:quickload :staple-markdown)

(loop :while (null (find-package "STAPLE")) :do
     (sleep 1))

(staple:generate :method-hooks :packages '(:method-hooks)
                 :if-exists :supersede
                 :documents (list (asdf:system-relative-pathname :method-hooks "README.md"))
)
