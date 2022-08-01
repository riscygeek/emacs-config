;;; Source: https://github.com/arthurmco/insert-header-guard

;;; Insert a C header guard in a file
;;; Copyright 2017 Arthur M
;;;
;;; This code also requires variable 'enable-insert-header-guard' to be defined
;;; Useful when you want to only activate it in a binding (eg C/C++ modes)

;;; Code:

(defun insert-header-guard-enable ()
    "Enable header guard insertion."
    (setq enable-insert-header-guard t))

(defun is-header-guard-enabled ()
  "Check if header guard is enabled."
  (and (boundp 'enable-insert-header-guard) enable-insert-header-guard))

(defun insert-header-guard (guardstr)
  "Insert a header guard named GUARDSTR in a c++ file."
  (interactive "sHeader guard: ")
  (if (is-header-guard-enabled)
      (progn
	  (insert "#ifndef " guardstr "\n")
	  (insert "#define " guardstr "\n\n")
	  (save-excursion
	    (insert "\n\n")
	    (insert "#endif // " guardstr)))
    (error "Header guards are not enabled here")))

(provide 'insert-header-guard)
;;; insert-header-guard.el ends here
