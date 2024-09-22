

(use-package eglot
  :ensure t)

(use-package eglot-java
  :ensure t
  :custom
  (eglot-report-progress nil) ; TODO: find out how to get mode-local vars
  )

;; notes:
;; git create repo, makefile, project-compile





(provide 'esper-lang-java)
