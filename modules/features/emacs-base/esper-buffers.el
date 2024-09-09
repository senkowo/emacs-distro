;; -*- lexical-binding: t; -*-

(C-c-define-key
  "k" 'kill-current-buffer
  "b" '(:ignore t :which-key "buffer")
  "bk" 'kill-current-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bo" '(mode-line-other-buffer :which-key "last-buffer")
  "bb" (pcase (featurep-first '(consult))
	 ('consult 'consult-buffer)
	 (_        'switch-to-buffer)))

;;; TODO: pcase featurep-first shit (move to setup macros n shit)

;;; TODO: add lexical scoping to every file (or else featurep might not work)

(provide 'esper-buffers)

