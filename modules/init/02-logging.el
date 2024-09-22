

;; print startup time after Emacs starts up:
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s seconds with %d garbage collections."
		     (emacs-init-time "%.2f")
		     gcs-done)))

(provide '02-logging)
