
(defvar esper-package-manager-available '(straight package)
  "List of available package managers.")

(defvar esper-package-manager-default (car esper-package-manager-available)
  "The fallback package manager if not specified.")

(unless (member esper-package-manager esper-package-manager-available)
  (display-warn '20-package-managers
		(format "%s%S. %s%S"
			"The value of `esper-package-manager' is invalid: "
			esper-package-manager
			"Defaulting to value of `esper-package-manager-default': "
			esper-package-manager-default))
  (setq esper-package-manager esper-package-manager-default))

;; load the selected package manager 
(pcase esper-package-manager
  ('straight (+load "03--package-manager-straight"))
  ('package  (+load "03--package-manager-package")))


(provide '03-package-manager)
