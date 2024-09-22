;; -*- lexical-binding: t; -*-

(use-package dashboard
  :ensure t
  :demand t ;; activate it now
  :bind (:map dashboard-mode-map
	      ("n" . dashboard-next-line)
	      ("p" . dashboard-previous-line))
  :config
  (add-hook 'after-init-hook
	    (lambda ()
  	      (dashboard-insert-startupify-lists))) ;; Display useful lists of items
  (add-hook 'emacs-startup-hook
	    (lambda ()
              (switch-to-buffer dashboard-buffer-name)
              (goto-char (point-min))
              (redisplay)
              (run-hooks 'dashboard-after-initialize-hook)))
  
  ;; if server-mode, when create new emacsclient frame, switch to dashboard
  (eval-after-load 'server
    (add-hook 'server-after-make-frame-hook
	      (lambda ()
		(switch-to-buffer dashboard-buffer-name)))))


(provide 'esper-dashboard)
