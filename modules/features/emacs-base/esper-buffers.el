
(C-c-define-key
  "k" 'kill-current-buffer
  "b" '(:ignore t :which-key "buffer")
  "bk" 'kill-current-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bo" 'meow-last-buffer
  "bb" 'counsel-switch-buffer
  "br" 'read-only-mode)
