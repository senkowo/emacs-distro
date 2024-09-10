;; -*- lexical-binding: t; -*-

(require 'org-agenda)


(defun ri/org-agenda-files-search-recursively (lst)
  "For each path in LST, recursively find all org files.
If an item in LST is not a directory, process as a single file.
If LST is not a list, just process that one path."
  (mapcan (lambda (path)
	    (if (file-directory-p path)
		(directory-files-recursively
		 path org-agenda-file-regexp)
	      (list path)))
	  (flatten-tree lst)))

(defun ri/org-agenda-dirs-search-recursively (lst)
  "For each path in LST, recursively find all directories of org files.
Uses `ri/org-agenda-files-search-recursively' to identify org files."
  (let ((all-matching-files (ri/org-agenda-files-search-recursively lst))
	(ret '()))
    (message "%S" all-matching-files)
    (mapcar (lambda (f)
	      (message "f: %S" f)
	      (message "ret: %S, listp: %S" ret (listp ret))
	      (let ((parent-dir (file-name-parent-directory f)))
		(message "time to member: %S" parent-dir)
		(unless (member parent-dir (ensure-list ret))
		  (setq ret (append ret (list parent-dir))))))
	    all-matching-files)))


(setq org-agenda-files
      (append (list "~/Notes/School/general.org"
		    "~/Notes/School/classes/assignments.org"
		    "~/Notes/org/agenda.org")))

;; tweak keybinds for better compat with dvp
;; TODO: find better solutions... better way to swap two keys?
(define-key org-agenda-mode-map (kbd "&") #'org-agenda-todo)
(define-key org-agenda-mode-map (kbd ")") #'org-agenda-todo)
(define-key org-agenda-mode-map (kbd "(") #'org-agenda-todo)


(provide 'esper-workflow)
