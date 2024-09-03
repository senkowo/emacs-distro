
;;
;;; Notes: ;;; ----------
;;
;; - Basically necessary:
;; - Good default keybind tweaks
;; - Good default functionality tweaks
;; - Good default UI tweaks
;; - Good default GUI menu tweaks
;; - Good default file/buffer tweaks
;; - Theming
;; - Very recommended default programs
;;
;; - Overarching:
;;   - completion
;;   - language server
;; - Keybinds:
;;   - meow, vim, godmode
;; - Emacs generic:
;;   - windows
;;   - buffers
;;   - qualityoflife(improveexisting)
;;   - tools
;; - UI:
;;   - modeline
;;   - line-numbers
;; - org:
;;   - org
;;   - workflow
;; - dired
;; - magit
;; - terminals
;; - tutorial
;; - customization:
;;   - transparency
;;   - dashboard
;;   - fun

(setq ri/is-guix-system t)

;;;; ############### Keybinds: ###############
(+load "esper-meow")
;; TODO: Evil

;;;; ############### Overarching: ###############
;; Minibuffer completion frameworks:
(+load "esper-completion")
;; TODO: Language server

;;;; ############### Emacs generic: ###############
(+load "esper-windows")
(+load "esper-buffers")
(+load "esper-improve-existing")

;;;; ############### UI: ###############
(+load "esper-modeline")
(+load "esper-line-numbers")

;;;; ############### org: ###############
(+load "esper-org")
(+load "esper-workflow")

;;;; ############### applications: ###############
(+load "esper-magit")
(+load "esper-terminals")
(+load "esper-dired")
;; TODO: dirvish
;; (+load "esper-dirvish")

;;;; ############### tutorial/guide: ###############
(+load "esper-tutorials")

;;;; ############### customization: ###############
(+load "esper-transparency")
(+load "esper-dashboard")
(+load "esper-fun")

;;;; ############### code: ###############
(+load "esper-lang-general")

(+load "esper-lang-lisp")
(+load "esper-lang-lisp-adv")
(+load "esper-lang-scheme")

;;;; ############### misc/temp: ###############
(+load "esper-server")
;; tODO: bring over old ri-rest
(+load "esper-rest")



