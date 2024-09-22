;; -*- lexical-binding: t; -*-

(use-package dired :straight (:type built-in)
  :ensure nil
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-dwim-target t) ; auto select dir to move to if another dired window open.
  (delete-by-moving-to-trash t)
  :bind (:map dired-mode-map
	      ;; only for dvorak
	      ("h" . dired-up-directory)
	      ("s" . dired-open-file)))

;; reuse dired buffers
;; TODO: implement reusing dired buffers
;; (setup dired-single)

;;; --- misc: ----

;; Icons to dired
(use-package all-the-icons-dired
  :ensure t
  ;; TODO: must disable hook if dirvish-override-dired-mode eq nil
  ;; :config
  ;; (add-hook 'dired-mode-hook ;; TODO: breaks if dirvish is disabled
  ;; 	    (lambda ()
  ;; 	      (all-the-icons-dired-mode dirvish-override-dired-mode)))
  )

;; Open certain files using external programs
(use-package dired-open
  :ensure t
  :config
  (setq dired-open-extensions
        '(("mkv" . "mpv")
          ;; ("png" . "feh")
          ("docx" . "libreoffice"))))

;; TODO: add dired-hide-dotfiles here

;; custom function for sorting
(defun ri/dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://xahlee.info/emacs/emacs/dired_sort.html'
Version: 2018-12-23 2022-04-07"
  (interactive)
  (if (equal major-mode 'dired-mode)
      (let (xsortBy xarg)
	(setq xsortBy (completing-read "Sort by:" '( "date" "size" "name" )))
	(cond
	 ((equal xsortBy "name") (setq xarg "-Al "))
	 ((equal xsortBy "date") (setq xarg "-Al -t"))
	 ((equal xsortBy "size") (setq xarg "-Al -S"))
	 ((equal xsortBy "dir") (setq xarg "-Al --group-directories-first"))
	 (t (error "logic error 09535" )))
	(dired-sort-other xarg ))
    (message "Not in dired buffer.")))

;;; --- Bookmarks: ---
;; TODO: move elsewhere?

(setq bookmark-save-flag 1) ; save bookmarks to file after every change
(C-c-define-key
  "sm" 'consult-bookmark
  "sM" 'bookmark-set)

;;; --- leader key defs: ----

(C-c-define-key
  "d"  '(:ignore t :which-key "dired")
  "dd" 'dired
  "dj" 'dired-jump
  ;; "dh" 'ri/dired-hide-dotfiles-mode-toggle ; TODO!
  "ds" 'ri/dired-sort)


(provide 'esper-dired)
