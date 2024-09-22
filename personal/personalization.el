
;;; Guix ---

(use-package guix
  :ensure t
  :demand t
  :after general
  :general (C-c-define-key
	     "G" '(:ignore t :which-key "Guix")
	     "Gg" 'guix
	     "Gp" '(guix-packages-by-name :which-key "search packages"))
  :config
  ;; TODO: depends of system package emacs-guix
  (add-hook 'scheme-mode-hook 'guix-devel-mode))

;;; Vertico ---

(global-set-key (kbd "C-s") 'consult-line)

;;; Dashboard ---

;;; Files ---

(use-package hydra
  :ensure t)

(defhydra my-directory-hydra (:color blue :hint nil)
  ("d" (lambda () (interactive) (dired "~/Document")) "Documents" :exit t)
  ("p" (lambda () (interactive) (dired "~/Projects")) "Projects" :exit t)
  ("q" nil "quit" :exit t))

;;; Development ---

(defun ri/insert-provide-sexp ()
  (interactive)
  (let* ((basename (file-name-base))
	 (search-regex "^(provide .*)")
	 (new-string (format "(provide '%s)" basename))
	 (found nil))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward search-regex nil t)
        (replace-match new-string)
        (setq found t)
	(message "Replaced for %s" basename))
      (unless found
	(goto-char (point-max))
	(newline)
	(insert new-string)
	(message "Doesn't exist, inserted %s for %s" new-string basename)))))


