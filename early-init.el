

;; prevent package.el from loading
(setq package-enable-at-startup nil)

;; Initial color at startup:
;; - currently customized for kaolin-shiva theme
;; - TODO: create a custom set-themes function that modifies
;;   this line afterwards?
;; - use external file for storing value?
;; TODO: error every new emacsclient has these colors.
;; (set-face-attribute 'default nil :foreground "#fcefe6" :background "#2a2028")

(defvar esper-overwrite-alist nil
  "An alist to specify variables to overwrite in init.el.
This should be customized in \"early-config.el\".")


;;; Move to early-config.el

(setq esper-overwrite-alist
      `((esper-benchmark . t)))
