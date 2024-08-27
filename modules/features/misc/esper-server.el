
(defun ri/start-server-if-not-running ()
  (unless (or (processp server-process)
	      (server-running-p))
    (server-start)
    (message "Emacsclient Server started!")))

(add-hook 'after-init-hook #'ri/start-server-if-not-running)
