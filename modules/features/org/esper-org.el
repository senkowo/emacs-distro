
;;; Custom commands

(defun ri/org-insert-subheading-respect-content ()
  "Insert new subheading after the current heading's body."
  (interactive)
  (org-meta-return)
  (org-metaright))

;;; Main org configuration

(defun ri/org-font-setup ()
  "Configures fonts in current buffer for org mode."
  (interactive)
  ;; (set-face-attribute 'default nil :font "Liberation Mono" :height 100)
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Liberation Mono" :height (cdr face))))

;; GREAT docs https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html
(use-package org :straight (:type built-in)
  :custom
  (org-hide-emphasis-markers nil) ; hide formatting chars (* / ~ = etc)
  (org-ellipsis " ▼") ; symbol for folded content
  (org-startup-indented t) ; indent headings and body
  (org-startup-folded 'showall) ; default startup folding
  :hook
  (org-mode . ri/org-font-setup)
  (org-mode . visual-line-mode)
  :bind (:map org-mode-map
	      ("C-M-<return>" . ri/org-insert-subheading-respect-content))
  :config
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("unix" . "src conf-unix"))
  (add-to-list 'org-structure-template-alist '("conf" . "src conf"))
  (add-to-list 'org-structure-template-alist '("clang" . "src c"))
  (add-to-list 'org-structure-template-alist '("gcc" . "src c"))
  (add-to-list 'org-structure-template-alist '("cl" . "src common-lisp"))
  (add-to-list 'org-structure-template-alist
               '("mani" . "src scheme :noweb-ref packages :noweb-sep \"\""))
)

;;; Misc org packages

;; auto-tangle.el

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

;; toc-org

;; (use-package toc-org
;;   :hook (org-mode . toc-org-mode))

;; org-pomodoro

(use-package org-pomodoro
  :bind ("C-c o p" . org-pomodoro))

;; org-download

(use-package org-download)

;; iscroll (not work with evil by default)

(use-package iscroll
  :hook (org-mode . iscroll-mode))

;; org LaTeX

;; (use-package auctex
;;   :ensure t)

;; TODO: causes org bugs?
;; (use-package org-fragtog
;;   :hook
;;   (org-mode . org-fragtog-mode))

;; anki-editor

(use-package anki-editor)
