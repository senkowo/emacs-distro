

(use-package geiser
  :ensure t
  :mode ("\\.sld\\'" . scheme-mode)
  :hook ((scheme-mode . guix-devel-mode)
	 (scheme-mode . flycheck-mode))
  :custom
  (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-implementations-alist '(((regexp "\\.scm$") guile))))

(use-package geiser-guile
  :ensure t
  :after geiser)
