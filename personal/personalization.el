

(use-package guix
  :ensure t
  :after general
  :general (C-c-define-key
	     "G" '(:ignore t :which-key "Guix")
	     "Gg" 'guix
	     "Gp" '(guix-packages-by-name :which-key "search packages")))

