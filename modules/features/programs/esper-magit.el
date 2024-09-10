;; -*- lexical-binding: t; -*-

;; git client
(use-package magit
  :ensure t
  ;; :bind ("C-M-;" magit-status)	; TODO: even used???
  ;; TODO: is this necessary?
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (C-c-define-key
    "v" '(:ignore t :which-key "magit")
    "vv" 'magit))

;; highlight todo's and other keywords in magit
;; TODO: broken package, "error in process sentinel: Wrong type argument: stringp, nil"
;; (use-package magit-todos
;;   :ensure t
;;   :after magit
;;   :config (magit-todos-mode 1))

;; Access github and gitlab commits/issues/pr/etc from Emacs:
;; (use-package forge
;;   :ensure t
;;   :after magit)


(provide 'esper-magit)
