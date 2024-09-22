;; -*- lexical-binding: t; -*-

(use-package geiser
  :ensure t
  :mode ("\\.sld\\'" . scheme-mode)
  :hook (scheme-mode . flycheck-mode)
  :custom
  (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-implementations-alist '(((regexp "\\.scm$") guile))))

(use-package geiser-guile
  :ensure t
  :after geiser)


(provide 'esper-lang-scheme)
