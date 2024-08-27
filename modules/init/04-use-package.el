

(defun esper--use-package-init-straight ()
  (unless (package-installed-p 'use-package)
    (straight-use-package 'use-package))
  (setq straight-use-package-by-default t))

(defun esper--use-package-init-package ()
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package))

;; install use-package and make it use the package manager by default
(pcase esper-package-manager
  ('straight (esper--use-package-init-straight))
  ('package  (esper--use-package-init-package))
  (_ (error "`esper-package-manager' \"%S\" doesn't match any cases"
	    esper-package-manager)))
