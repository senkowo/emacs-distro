

(defun +warn-error (type thing err)
  "Display a warning of level :error.
TYPE is a symbol for the group name.
THING is a string specifying the process that went wrong.
ERR is an error value with the structure (ERROR-SYMBOL . DATA)."
  (display-warning type
		   (format "%s: %s" thing (error-message-string err))
		   :error))

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

(defun esper--keyword-parser (body)
  "Parses BODY and returns an alist of keyword-body pairs."
  (let (cur current-keyword init-body config-body)
    (while body
      (if (keywordp (setq current-keyword (pop body)))
	  (progn
	    (while (and body (not (keywordp (car body))))
	      (setq cur (pop body))
	      (pcase current-keyword
		(:init (push cur init-body))
		(:config (push cur config-body)))))
	(error "Expecting keyword")))
    (list `(:init ,(reverse init-body))
	  `(:config ,(reverse config-body)))))

(defmacro +require (feature &rest body)
  "A use-package-like macro around `require'.
Adds optional benchmarking, soft error-handling, and support for keywords like
in `use-package'. The keywords currently supported are :init and :config.
FEATURE is the library to `require' and must be a symbol.
BODY is for optional keywords and their arguments.
The keywords and their arguments are parsed using `esper--keyword-parser'."
  (declare (indent defun))
  (let ((wrapper (if esper-benchmark `(+benchmark-progn ,feature 'require) '(progn))))
    `(,@wrapper
      (condition-case-unless-debug e
	  ,(let* ((parsed (esper--keyword-parser body))
		  (init-body (car (alist-get :init parsed)))
		  (config-body (car (alist-get :config parsed))))
	     `(progn
		,@(append init-body
			  `((require ,feature))
			  config-body)))
	(error (+warn-error '+require ,feature e))))))

(defmacro +load (file &rest body)
  "A use-package-like macro around `load'.
Adds optional benchmarking, soft error-handling, and support for keywords like
in `use-package'. The keywords currently supported are :init and :config.
FILE is the file or library to `load' and must be a string.
BODY is for optional keywords and their arguments.
The keywords and their arguments are parsed using `esper--keyword-parser'."
  (declare (indent defun))
  (let ((wrapper (if esper-benchmark `(+benchmark-progn ,file 'require) '(progn))))
    `(,@wrapper
      (condition-case-unless-debug e
	  ,(let* ((parsed (esper--keyword-parser body))
		  (init-body (car (alist-get :init parsed)))
		  (config-body (car (alist-get :config parsed))))
	     `(progn
		,@(append init-body
			  `((load ,file))
			  config-body)))
	(error (+warn-error '+load ,file e))))))


(provide '00-module-loading-macros)
