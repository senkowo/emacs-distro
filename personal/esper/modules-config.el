

;;;; Move these into new modules later: ;;;;



;;; Good default keybind tweaks: ;;; ----------

;; move up/down with cursor in place
(setq scroll-preserve-screen-position nil)
(global-set-key (kbd "M-p") (kbd "M-- 1 C-v"))
(global-set-key (kbd "M-n") (kbd "M-- 1 M-v"))

(defalias 'yes-or-no-p 'y-or-n-p)

;; By default, the <escape> key will actuate the Meta/Alt key.
;; This makes <escape> work more like expected.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; Some leader-key stuff: ;;; ----------

(C-c-define-key
  "s" '(:ignore t :which-key "save")
  "q" '(:ignore t :which-key "quit/session")
  "qq" '(save-buffers-kill-terminal :which-key "quit"))

;; save/jump to position/register
(C-c-define-key
  "sr" 'point-to-register
  "sR" 'jump-to-register)

(C-c-define-key
  "f"  '(:ignore t :which-key "files")
  "fr" '(recentf :which-key "recent files")
  "ff" '(find-file :which-key "find-file"))



;;; Good default functionality tweaks: ;;; ----------

(recentf-mode 1) ; show recent files when viewing files (counsel enables by def).
(save-place-mode 1) ; go to previous location in file when reopening.


;; disable bell
;; (setq ring-bell-function 'ignore)



;;; UI tweaks: ;;; ----------

;; improve scrolling functionality (when cursor at bottom, screen will
;; move along a few lines, then then jump forward half a page).
;; TODO: try to find more natural way of scrolling.
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ; scroll when using mouse wheel.
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling.
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse regardless of focus.
(setq scroll-conservatively 0) ; move window when moving off screen.
(setq scroll-margin 0) ; margin before scroll at top and bottom of screen.
(setq scroll-step 1) ; keyboard scroll one line at a time.
(setq use-dialog-box nil) ; (change to nil) make things like yes or no prompts dialogue boxes.


;;; gui tweaks: ;;; ----------

;; disable ui
(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1)   ; disable the toolbar
(tooltip-mode 1)     ; disable tooltips
(set-fringe-mode 10) ; give some breathing room
(menu-bar-mode -1)   ; disable menu bar


;;; file tweaks: ;;; ----------

;; TODO: move this into a better place?
;; (setq fill-column 70) ; shadowed by visual-fill-column if visual-fill-column-mode is non-nil.

;; Update dired and file buffers when changed in filesystem.
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;;; Theming: ;;; ----------

(use-package doom-themes)
(use-package kaolin-themes)
(use-package ef-themes)
;; (load-theme 'ef-trio-dark t)
(load-theme 'ef-rosa t)

;;; Very recommended default programs: ;;; ----------

(use-package avy
  :ensure t)
