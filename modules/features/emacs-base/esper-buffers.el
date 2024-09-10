;; -*- lexical-binding: t; -*-

(C-c-define-key
  "k" 'kill-current-buffer
  "b" '(:ignore t :which-key "buffer")
  "bk" 'kill-current-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bo" '(mode-line-other-buffer :which-key "last-buffer")
  "bb" 'switch-to-buffer)

(eval-after-load 'consult
  (C-c-define-key
    "bb" 'consult-buffer))

;;; TODO: add lexical scoping to every file (or else featurep might not work)

(provide 'esper-buffers)
