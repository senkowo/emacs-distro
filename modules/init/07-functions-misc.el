
(defun featurep-first (features)
  (cl-some (lambda (pkg)
	     (when (featurep pkg)
	       pkg))
	   (flatten-tree features)))

(provide '07-functions-misc)
