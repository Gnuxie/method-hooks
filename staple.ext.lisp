(defun strip-to-relative (some-path-string)
  (let ((system-dir-name "method-hooks"))
    (let ((chop-point (search system-dir-name some-path-string)))
      (format *standard-output* "~a~%" (subseq some-path-string (+ chop-point (length system-dir-name))))
      (subseq some-path-string (+ chop-point (length system-dir-name))))))

(defun format-source-for-repo (source)
  (format NIL "http://gitlab.com/Gnuxie/method-hooks/blob/master~a#L~a"
          
          (strip-to-relative (namestring (getf source :file)))
    (getf source :row)))

(defmethod staple:resolve-source-link (source (page staple:definitions-index-page))
  (format-source-for-repo source))

(defmethod staple:resolve-source-link (source (page staple:system-page))
  (format-source-for-repo source))
