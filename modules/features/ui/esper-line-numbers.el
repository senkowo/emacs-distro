
(global-display-line-numbers-mode t)  ;; add line numbers
(column-number-mode 1) ; (columns on modeline)

(defvar display-line-numbers-mode-exclude
  '(org-mode-hook
    dired-mode-hook
    term-mode-hook
    vterm-mode-hook
    shell-mode-hook
    eshell-mode-hook
    image-minor-mode-hook
    doc-view-mode-hook
    treemacs-mode-hook)
  "List of mode hooks to exclude line-numbers-mode")

;; line number mode exceptions
;; TODO: rewrite all this into disabling mode per pkg hook.
(dolist (mode display-line-numbers-mode-exclude)
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
