
;;
;; Autoloads: hook, commands, bind, bind-keymap, mode, interpreter
;; 
;; Load now: demand
;; Autoload: defer
;;
;; Useful:
;;   :init {before loading, reguardless of deferred}
;;   :after (:all foo bar)
;;   :after (:any foo bar)
;;   :requires {equal to :if (featurep foo) }
;;   :autoload [non-interactive function]
;;   :preface
;;   M-x describe-personal-keybindings
;;   [Info: A.2 How to create an extension keyword]


;; install use-package and make it use the package manager by default
(pcase esper-package-manager
  ('straight
   (progn (unless (package-installed-p 'use-package)
	    (straight-use-package 'use-package))
	  (setq straight-use-package-by-default t)))
  ('package
   (progn (unless (package-installed-p 'use-package)
	    (package-install 'use-package))
	  (require 'use-package)))
  (_
   (error "`esper-package-manager' \"%S\" doesn't match any cases"
	  esper-package-manager)))

;; have better debug info when emacs is ran with --debug-init
(when init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        use-package-compute-statistics t
        debug-on-error t))


(provide '04-use-package)
