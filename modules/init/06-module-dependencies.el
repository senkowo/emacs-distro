

;;; absolutely necessary for other modules: ;;; ----------

;; - general.el

(use-package general
  :ensure t
  :config
  (general-create-definer C-c-define-key
    :prefix "C-c"))

(use-package which-key
  :ensure t
  :diminish t				; TODO: check if this works
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))


(provide '06-module-dependencies)
