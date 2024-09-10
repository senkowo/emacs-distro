;;; init.el --- starting init file -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is not meant to be edited.

;; Stages:
;; - lower gc freq
;; - set important paths
;; - add to load-path
;;

;; TODO list:
;; - elfeed
;; - java env
;; - evil mode
;; - 

;;; Code:

;;; Preliminary:

;; check if emacs version is supported:
(let ((min-ver 28))
  (when (< emacs-major-version min-ver)
    (error "Emacs version %s is not supported, muts be %s or higher" emacs-version min-ver)))

;;; Init Settings:

;; lower gc freq for init:
(setq gc-cons-threshold (* 50 1000 1000))

;; TODO: the one below can be put elsewhere (second is maybe)

;; print startup time after Emacs starts up:
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s seconds with %d garbage collections."
		     (emacs-init-time "%.2f")
		     gcs-done)))

;; raise gc freq after Emacs initializes:
(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 2 1000 1000))))


;;; Define important variables:

;; Directories:
(setq user-emacs-directory
      (expand-file-name (if load-file-name
			    (file-name-directory load-file-name)
			  user-emacs-directory)))

(defvar esper-modules-dir (file-name-concat user-emacs-directory "modules")
  "Path to the modules directory.")
(defvar esper-personal-dir (file-name-concat user-emacs-directory "personal")
  "Path to the personal directory.")
(defvar esper-vendor-dir (file-name-concat user-emacs-directory "vendor")
  "Path to the vendor directory.")

;; Files:
(defvar esper-early-config-file (file-name-concat esper-personal-dir "esper/early-config.el")
  "Path to the userside early-init config file..")
(defvar esper-init-config-file (file-name-concat esper-personal-dir "esper/init-config.el")
  "Path to the userside init configuration file.")
(defvar esper-modules-config-file (file-name-concat esper-personal-dir "esper/modules-config.el")
  "Path to the userside modules config file.")

;; Init modules vars:
(defvar esper-init-modules
  '(01-good-def-vars
    02-logging
    03-package-manager
    04-use-package
    05-no-littering
    06-module-dependencies)
  "The init modules to be loaded.
Should be edited in `esper-init-config-file'")
(defvar esper-init-modules-default esper-init-modules
  "The default init modules to be loaded (fallback).")

(defvar esper-exclude-files
  (list esper-early-config-file esper-init-config-file)
  "List of files to exclude from loading.")

;; Package manager:
(defvar esper-package-manager 'straight
  "The package manager to use.")

;; where changes made through the customize UI will be stored
(defvar custom-file (expand-file-name "custom.el" esper-personal-dir))

;; location to keep themes
(setq custom-theme-directory (file-name-concat user-emacs-directory "misc/themes"))

;; Misc:
;; TODO: issue: benchmark variable overwrite has to happen in early-config... init-config instead somehow?
(defvar esper-benchmark (getenv "ESPER_BENCHMARK")
  "Benchmark module load times in macros `+load' and `+require'.")

;;; Overwrite the default variables:

;; This is done to allow for flexible customization of this file without direct editing.
(defun esper-overwrite-variables ()
  "Change the value of variables according to `esper-overwrite-alist'.
For each alist pair, its car is the variable name and cdr is the new value."
  (dolist (pair esper-overwrite-alist)
    (let ((var (car pair))
	  (value (cdr pair)))
      (set var value))))

(esper-overwrite-variables)


;;; Add paths to 'load-path:

(defun add-subdirs-to-load-path (path)
  "Add PATH and all its subdirs to the `load-path'."
  (when path
    (let ((default-directory path))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))))

(add-subdirs-to-load-path esper-modules-dir)
(add-subdirs-to-load-path esper-personal-dir)
(add-subdirs-to-load-path esper-vendor-dir)


;;; Define fancy module-loading functions:

(defmacro +benchmark-progn (feature action &rest body)
  "A slight modification to the `benchmark-progn' macro.
It now prints FEATURE when printing the time taken to eval BODY.
ACTION can be either \\='require or \\='load, which prepends FEATURE with either
\"Required: \" or \"Loaded: \". If neither match, print \\=`\",ACTION: \"."
  (declare (debug t) (indent defun))
  (let ((value (make-symbol "value"))
	(start (make-symbol "start"))
	(gcs (make-symbol "gcs"))
	(gc (make-symbol "gc")))
    `(let ((,gc gc-elapsed)
	   (,gcs gcs-done)
           (,start (current-time))
           (,value (progn
                     ,@body)))
       (message " Elapsed time: %fs, %s%s%s"
                (float-time (time-since ,start))
		(cond ((eq ,action 'require)
		       "Required: '")
		      ((eq ,action 'load)
		       "Loaded: ")
		      (t (format "%s: " ,action)))
		,feature
                (if (> (- gcs-done ,gcs) 0)
                    (format " (%fs in %d GCs)"
	                    (- gc-elapsed ,gc)
	                    (- gcs-done ,gcs))
                  ""))
       ;; Return the value of the body.
       ,value)))

(defun +warn-error (type thing err)
  "Display a warning of level :error.
TYPE is a symbol for the group name.
THING is a string specifying the process that went wrong.
ERR is an error value with the structure (ERROR-SYMBOL . DATA)."
  (display-warning type
		   (format "%s: %s" thing (error-message-string err))
		   :error))

(defun +load (&rest modules)
  "Loads the module(s) with soft error-handling and optional benchmarking."
  (setq modules (flatten-tree modules))
  (mapc (lambda (mod)
	  (let ((body `(condition-case-unless-debug e
			   (load ,mod)
			 (error (+warn-error '+load ,mod e)))))
	    (if esper-benchmark
		(+benchmark-progn mod 'load
		  (eval body))
	      (eval body))))
	modules))

;; (defun +require (&rest modules)
;;   (setq modules (flatten-tree modules))
;;   (mapc (lambda (mod)
;; 	  (let ((body `(condition-case-unless-debug e
;; 			   (require ',mod)
;; 			 (error (+warn-error '+require ,mod e)))))
;; 	    (if esper-benchmark
;; 		(+benchmark-progn mod 'require
;; 		  (eval body))
;; 	      (eval body))))
;; 	modules))

;; I GET THE ISSUE: load init modules is failing

;; (defun +require (modules)
;;   (setq modules (flatten-tree modules))
;;   (mapcar (lambda (mod)
;; 	    (let ((wrapper (if esper-benchmark `(+benchmark-progn ',mod 'require) '(progn))))
;; 	      (eval
;; 	       `(,@wrapper
;; 		 (condition-case-unless-debug e
;; 		     (require ',mod)
;; 		   (error (+warn-error '+require ',mod e)))))))
;; 	  modules))

;; (defun +require--parse-body (body)
;;   "Parses BODY and returns a cons of init-body and config-body."
;;   (let (cur current-keyword init-body config-body)
;;     (while body
;;       (if (keywordp (setq current-keyword (pop body)))
;; 	  (progn
;; 	    (while (and body (not (keywordp (car body))))
;; 	      (setq cur (pop body))
;; 	      (pcase current-keyword
;; 		(:init (push cur init-body))
;; 		(:config (push cur config-body)))))
;; 	(error "Expecting keyword")))
;;     (cons init-body config-body)))

(defmacro +require (feature &rest body)
  "A massive macro around `require'.
Adds optional benchmarking, soft error-handling, and keywords like
`use-package'. The only keywords supported are :init and :config.
FEATURE is what will be passed to `require' and must be a symbol.
BODY is for optional keywords and their arguments."
  (declare (indent defun))
  (let ((wrapper (if esper-benchmark `(+benchmark-progn ,feature 'require) '(progn))))
    `(,@wrapper
      (condition-case-unless-debug e
	  ,(let (cur current-keyword init-body config-body)
	     (while body
	       (if (keywordp (setq current-keyword (pop body)))
		   (progn
		     (while (and body (not (keywordp (car body))))
		       (setq cur (pop body))
		       (pcase current-keyword
			 (:init (push cur init-body))
			 (:config (push cur config-body)))))
		 (error "Expecting keyword")))
	     `(progn
		,@(append (reverse init-body)
			  `((require ,feature))
			  (reverse config-body))))
	(error (+warn-error '+require ,feature e))))))

(defun featurep-first (features)
  (cl-some (lambda (pkg)
	     (when (featurep pkg)
	       pkg))
	   (flatten-tree features)))

;;; Load optional config file

;; In `esper-init-config-file', you can edit the init modules to use,
;; the package manager to use, etc etc.
(when (file-exists-p esper-init-config-file)
  (+load esper-init-config-file))

;;; Require init modules

(mapc (lambda (mod)
	(+require mod))
      esper-init-modules)

;;; Load modules based on config file:

(when (file-exists-p esper-modules-config-file)
  (+load esper-modules-config-file))

;;; Load all user config files lexigraphically:

(let ((all-files (directory-files esper-personal-dir 't "^[^#\.].*\\.el$"))
      (exclude esper-exclude-files))
  (dolist (ex exclude)
    (setq all-files (delete ex all-files)))
  (+load all-files))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(geiser-guile geiser paredit flycheck fireplace dashboard free-keys key-quiz dired-open all-the-icons-dired multi-vterm magit embark-consult embark marginalia consult vertico meow desktop-environment exwm mu4e vterm which-key))
 '(safe-local-variable-values '((flycheck-disabled-checkers emacs-lisp-checkdoc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
