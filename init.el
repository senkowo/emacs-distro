;;; init.el --- starting init file -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is not meant to be edited.

;; Stages:
;; - lower gc freq
;; - set important paths
;; - add to load-path
;;

;; TODO list:
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
  '("01-good-def-vars"
    "02-logging"
    "03-package-manager"
    "04-use-package"
    "05-no-littering")
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

(defun +load--warn-error (type thing err)
  "Display a warning of level :error.
TYPE is a symbol for the group name.
THING is a string specifying the process that went wrong.
ERR is an error value with the structure (ERROR-SYMBOL . DATA)."
  (display-warning type
		   (format "%s: %s" thing (error-message-string err))
		   :error))

(defalias '+require--warn-error '+load--warn-error)

(defun +load (&rest modules)
  "Loads the module(s) with soft error-handling and optional benchmarking."
  (setq modules (flatten-tree modules))
  (mapc (lambda (mod)
	  (let ((body `(condition-case-unless-debug e
			   (load ,mod)
			 (error (+load--warn-error '+load ,mod e)))))
	    (if esper-benchmark
		(benchmark-progn (eval body))
	      (eval body))))
	modules))

(defun +require (&rest modules)
  (setq modules (flatten-tree modules))
  (mapc (lambda (mod)
	  (let ((body `(condition-case-unless-debug e
			   (require ,mod)
			 (error (+require--warn-error '+require ,mod e)))))
	    (if esper-benchmark
		(benchmark-progn (eval body))
	      (eval body))))
	modules))

;;; Load init-config to tweak init-module loading:

;; In `esper-init-config-file', you can edit the init modules to use,
;; the package manager to use, etc etc.
(when (file-exists-p esper-init-config-file)
  (+load esper-init-config-file))

;; load init modules
(+load esper-init-modules)

;;; Load user-specified optional modules:

(when (file-exists-p esper-modules-config-file)
  (+load esper-modules-config-file))

;;; Load all user config files:

(let ((all-files (directory-files esper-personal-dir 't "^[^#\.].*\\.el$"))
      (exclude esper-exclude-files))
  (dolist (ex exclude)
    (setq all-files (delete ex all-files)))
  (+load all-files))

