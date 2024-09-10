;; -*- lexical-binding: t; -*-

;;; Packages:
;;
;; vertico
;; savehist
;; consult
;; orderless
;; marginalia
;; embark
;; embark-consult
;; project.el
;; cape

;; Vertico is a framework for minibuffer completion framework



;; TODO: corfu, kind-icon, wgrep?, consult-dir, cape
;; more at ~/code/cloned/daviwil-dots/.emacs.d/modules/dw-interface.el

;; TODO: vim keybinds for vertico completion shit (work on later) (also daviwil)

;; TODO: this module has high order priority (one less than keybinds)


(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  ;; :custom
  ;; (vertico-scroll-margin 0) ; Different scroll margin
  ;; (vertico-count 20) ; Show more candidates
  ;; (vertico-resize t) ; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ; Enable cycling for `vertico-next/previous'
  )

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)
  
  ;; Emacs 28 and newer: hide commands in M-x that do not work in the current mode.
  ;; (setq read-extended-command-predicate #'command-completion-default-include-p)
  
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; consult
(use-package consult
  :ensure t
  :demand t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ;; ("C-c )" . consult-kmacro)
	 
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ;; repeat-complex-command
         ("C-x b" . consult-buffer)	       ;; switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)	;; switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)		;; bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; project-switch-to-buffer
	 
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-store)
         ;; ("C-M-#" . consult-register)
         ("C-M-#" . consult-register-load)
	 
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; yank-pop
         ([remap Info-search] . consult-info)
	 
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)	 ;; goto-line
         ("M-g M-g" . consult-goto-line) ;; goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g O" . consult-org-heading)
	 
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find) ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s M" . consult-man)	; T for terminal
	 ("M-s I" . consult-info)
	 
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ;; isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; isearch-edit-string
         ("M-s l" . consult-line) ;; Needed by: consult-line to detect isearch
         ("M-s L" . consult-line-multi)	;; Needed by: consult-line to detect isearch
	 
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ;; next-matching-history-element
         ("M-r" . consult-history) ;; previous-matching-history-element
	 ))

;; fzf minibuffer completion, any order (https://github.com/oantolin/orderless)
;; TODO: read up on setting up for company and other packages.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Useful annotations in minibuffer completions
;; (https://github.com/minad/marginalia)
(use-package marginalia
  :ensure t
  ;; Cycle between different amounts of info shown in minibuffer
  :bind
  (:map minibuffer-local-map
	("M-A" . marginalia-cycle))
  (:map completion-list-mode-map
	("M-A" . marginalia-cycle))
  :init
  ;; must be loaded in :init (enable immediately, force load)
  (marginalia-mode 1))

;; Perform a variety of actions on a thing at point
;; https://github.com/oantolin/embark
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  
  :init
  ;; use embark for showing command prefix help
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Integration between embark and consult
;; (embark will load automatically if consult is found)
(use-package embark-consult
  :ensure t 
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(provide 'esper-vertico)
