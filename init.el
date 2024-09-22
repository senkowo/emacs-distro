;;; init.el --- starting init file -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is not meant to be edited.

;; Stages:
;; - Exit if appropriate
;; - GC freq
;; - Variables
;; - overwrite variables
;; - add paths to load-path
;; - Module loading macros
;; - load init config file
;; - Eval init-modules
;; - load init modules file
;; - load all user config files

;; TODO list:
;; - elfeed
;; - java env
;; - evil mode
;; - 

;;; Code:

;;; Exit if appropriate: -------------------------------------------------------

(let ((min-ver 28))
  (when (< emacs-major-version min-ver)
    (error "Emacs version %s is not supported, muts be %s or higher" emacs-version min-ver)))


;; lower gc freq for init and raise after initialization
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 2 1000 1000))))


;;; Variables: -----------------------------------------------------------------

(setq user-emacs-directory (expand-file-name user-emacs-directory))

;; Dirs:
(defvar esper-modules-dir (file-name-concat user-emacs-directory "modules"))
(defvar esper-personal-dir (file-name-concat user-emacs-directory "personal"))
(defvar esper-vendor-dir (file-name-concat user-emacs-directory "vendor"))

;; Files:
(defvar esper-early-config-file (file-name-concat esper-personal-dir "esper/early-config.el"))
(defvar esper-init-config-file (file-name-concat esper-personal-dir "esper/init-config.el"))
(defvar esper-modules-config-file (file-name-concat esper-personal-dir "esper/modules-config.el"))

;; Init modules vars:
(defvar esper-init-modules-list
  '(01-good-def-vars
    02-logging
    03-package-manager
    04-use-package
    05-no-littering
    06-module-dependencies
    07-functions-misc))
(defvar esper-init-modules-list esper-init-modules-list-default)

(defvar esper-exclude-files (list esper-early-config-file esper-init-config-file))

;; Package manager:
(defvar esper-package-manager 'straight
  "The package manager to use.")

;; where changes made through the customize UI will be stored
(defvar custom-file (expand-file-name "custom.el" esper-personal-dir))

;; location to keep themes
(setq custom-theme-directory (file-name-concat user-emacs-directory "misc/themes"))

;; TODO: issue: benchmark variable overwrite has to happen in early-config... init-config instead somehow?
(defvar esper-benchmark (getenv "ESPER_BENCHMARK")
  "Benchmark module load times in macros `+load' and `+require'.")


;;; Overwrite Variables: -------------------------------------------------------

;; This is done to allow for flexible customization of this file without direct editing.
(defun esper-overwrite-variables ()
  "Change the value of variables according to `esper-overwrite-alist'.
For each alist pair, its car is the variable name and cdr is the new value."
  (dolist (pair esper-overwrite-alist)
    (let ((var (car pair))
	  (value (cdr pair)))
      (set var value))))
(esper-overwrite-variables)


;;; Add paths to load-path: ----------------------------------------------------

(defun add-subdirs-to-load-path (path)
  "Add PATH and all its subdirs to the `load-path'."
  (when path
    (let ((default-directory path))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))))

(add-subdirs-to-load-path esper-modules-dir)
(add-subdirs-to-load-path esper-personal-dir)
(add-subdirs-to-load-path esper-vendor-dir)


;;; Module loading macros: -----------------------------------------------------

;; provides +require, +load, +benchmark-progn, and +warn-error
(require '00-module-loading-macros)

;;; Load init config file: -----------------------------------------------------

;; In `esper-init-config-file', you can edit the init modules to use,
;; the package manager to use, etc etc.
(when (file-exists-p esper-init-config-file)
  (+load esper-init-config-file))

;;; Eval init-modules: ---------------------------------------------------------

;; require init modules 01-06
(mapc (lambda (m)
	(+require m))
      esper-init-modules-list)

;;; Load modules config file

(when (file-exists-p esper-modules-config-file)
  (+load esper-modules-config-file))

;;; Load all user config files:

(let ((all-files (directory-files esper-personal-dir 't "^[^#\.].*\\.el$"))
      (exclude esper-exclude-files))
  (dolist (ex exclude)
    (setq all-files (delete ex all-files)))
  (mapc (lambda (f)
	  (+load f))
	all-files))
