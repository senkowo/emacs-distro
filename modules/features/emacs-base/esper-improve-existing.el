
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ("C-h h" . helpful-at-point)
  ("C-h H" . view-hello-file) ; command originally at "C-h h"
  ("C-h M" . which-key-show-major-mode)
  ("C-h E" . describe-keymap))

(defalias 'yes-or-no-p 'y-or-n-p)

;; Persist minibuffer history over Emacs restarts.
;; TODO: move elsewhere
(use-package savehist
  :ensure t
  :init
  (savehist-mode 1))
