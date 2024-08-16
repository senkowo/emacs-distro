;; Disable the flycheck module checker for all elisp files in this directory.
;; The errors are negligible when dealing with Emacs config files.

((emacs-lisp-mode . ((flycheck-disabled-checkers . (emacs-lisp-checkdoc)))))
