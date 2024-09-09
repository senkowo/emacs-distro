

(use-package paredit
  :ensure t
  :hook emacs-lisp-mode scheme-mode
  ;; :bind ("M-r" . nil)
  :bind (:map paredit-mode-map
	      ("M-s" . nil) ; paredit-splice-sexp
	      ))


(provide 'esper-lang-lisp-adv)
