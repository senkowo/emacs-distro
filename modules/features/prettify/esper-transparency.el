
(defvar my-transparency-value 80
  "Value used for transparency.")

(defun ri/native-transparency-supported? ()
  "Return true if native-transparency is supported."
  (and (version<= "29" emacs-version)))

(defun ri/toggle-transparency ()
  (interactive)
  (if (not (ri/native-transparency-supported?))
      "Native transparency is not supported."
    (let ((alpha (frame-parameter nil 'alpha-background)))
      (set-frame-parameter
       nil 'alpha-background
       (if (eql (cond ((numberp alpha) alpha)
                      ((numberp (cdr alpha)) (cdr alpha))
                      ;; Also handle undocumented (<active> <inactive>) form.
                      ((numberp (cadr alpha)) (cadr alpha)))
		100)
           my-transparency-value
	 100))))) ; For all new frames henceforth

;; TODO: add-to-list or set change default-frame-alist value (make external function?)
(defun ri/set-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (if (not (ri/native-transparency-supported?))
      "Native transparency is not supported"
    (set-frame-parameter (selected-frame) 'alpha-background value)))

;; Startup

(when (ri/native-transparency-supported?)
  (set-frame-parameter nil 'alpha-background my-transparency-value) ; For current frame
  (add-to-list 'default-frame-alist `(alpha-background . ,my-transparency-value)))
