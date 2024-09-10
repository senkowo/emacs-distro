;; -*- lexical-binding: t; -*-

;;; Vterm ----------

(use-package vterm
  :ensure t
  :custom
  (vterm-shell "bash")
  (vterm-buffer-name-string "vterm %s")
  (vterm-max-scrollback 10000)
  ;; (term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  :bind (:map vterm-mode-map
	      ("C-," . vterm-send-next-key)
	      ("C-=" . ri/hydra-vterm-scale/body)
	      ("C--" . text-scale-decrease)
	      ("C-+" . text-scale-increase)
	      ("C-p" . vterm-send-up)
	      ("C-n" . vterm-send-down))
  :config
  ;; TODO: move this to meow? What to do with interactions between packages? put in special category? special file with <package>-<package>-interaction?
  ;; enter Meow insert mode automatically when open
  ;; TODO: add no meow and vim variant!
  (when meow-global-mode
    (add-hook 'vterm-mode-hook (lambda ()
                                 (if meow-normal-mode
                                     (meow-insert-mode)))))
  ;; Fixes vterm compilation on Guix System.
  ;; (https://www.reddit.com/r/GUIX/comments/11gzhyu/
  ;;  how_to_compile_the_vterm_module_from_emacs_and/)
  (progn
    ;; TODO: what to do about this.... and ri/is-guix-system...
    (defun ri/vterm-link-guix-library-on-compile (f &rest r)
      "Advice to replace compiling vterm with linking to just symlinking the guix library"
      (let* ((guix-vterm-lib "~/.guix-extra-profiles/emacs/emacs/lib/vterm-module.so")
	     (dest-directory (file-name-directory (locate-library "vterm.el" t)))
	     (dest-file (file-name-concat dest-directory "vterm-module.so")))
	(if (f-exists-p guix-vterm-lib)
	    ;; if non-symlink (compiled library) is at dest, delete
	    (unless (file-symlink-p dest-file)
	      (delete-file dest-file))
	  ;; create symlink
          (make-symbolic-link guix-library dest-file t)
          (message "DEBUG: vterm guix library %s doesn't exist, cant compile" guix-vterm-lib))))
    (if ri/is-guix-system ; TODO: no need to do this shit :sob::sob::sob:
	(advice-add 'vterm-module-compile :around #'ri/vterm-link-guix-library-on-compile))))

;;; Multi-Vterm ----------

(use-package multi-vterm
  :ensure t
  :custom
  (multi-vterm-dedicated-window-height-percent 30))

;;; leader keys: ;;;

(C-c-define-key
  "at" 'vterm ; regular term
  "aT" 'multi-vterm ; create new multi-vterm
  "am" '(:ignore t :which-key "multi-vterm-control") ; keymap
  "amt" 'multi-vterm-project
  "amp" 'multi-vterm-prev
  "amn" 'multi-vterm-next
  "amm" 'multi-vterm-dedicated-toggle)


(provide 'esper-terminals)
