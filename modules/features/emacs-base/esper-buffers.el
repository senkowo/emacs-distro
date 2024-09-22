;; -*- lexical-binding: t; -*-

(C-c-define-key
  "k" 'kill-current-buffer	; orig: C-x k RET
  "b" '(:ignore t :which-key "buffer")
  "bk" 'kill-current-buffer	; orig: C-x k RET
  "bn" 'next-buffer		; orig: C-x <right>
  "bp" 'previous-buffer		; orig: C-x <left>
  "bo" '(mode-line-other-buffer :which-key "last-buffer")
  "bb" 'switch-to-buffer	; orig: C-x b
  "s" '(:ignore t :which-key "save")
  "ss" 'save-buffer)		; orig: C-x C-s

(eval-after-load 'consult
  (C-c-define-key
    "bb" 'consult-buffer))

;;; TODO: add lexical scoping to every file (or else featurep might not work)

(provide 'esper-buffers)
