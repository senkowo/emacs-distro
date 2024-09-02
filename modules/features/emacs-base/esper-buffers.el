;; -*- lexical-binding: t; -*-

(C-c-define-key
  "k" 'kill-current-buffer
  "b" '(:ignore t :which-key "buffer")
  "bk" 'kill-current-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bo" '(mode-line-other-buffer :which-key "last-buffer")
  "bb" (pcase (+featurep-first '(consult))
	 ('consult 'consult-buffer)
	 (_        'switch-to-buffer)))

;;; TODO: pcase featurep-first shit (move to setup macros n shit)

;;; TODO: add lexical scoping to every file (or else featurep might not work)

(defmacro +pcase-featurep-first (features &rest args)
  "A wrapper around `pcase' to find the first of FEATURES that exists.
Each symbol in FEATURES is evaluated with `featurep' until it returns t.
ARGS is treated like ARGS in `pcase'."
  `(pcase ',(cl-some (lambda (pkg)
		       (when (featurep pkg)
			 pkg))
		     (flatten-tree features))
     ,@args))

(defun +featurep-first (features)
  (cl-some (lambda (pkg)
	     (when (featurep pkg)
	       pkg))
	   (flatten-tree features)))

(pcase (featurep-first '(consult ivy))
  ('consult 'consult-buffer)
  ('ivy 'ivy-switch-to-buffer)
  (_ 'switch-to-buffer))
