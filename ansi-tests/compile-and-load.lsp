(in-package "COMMON-LISP-USER")

(eval-when (load eval compile)
  (unless (fboundp 'compile-file-pathname)
    (defun compile-file-pathname (pathname)
      (make-pathname :defaults pathname :type "o"))))

(defun compile-and-load (pathspec)
  "Find the file indicated by PATHSPEC, compiling it first if
   the associated compiled file is out of date."
  (let* ((pathname (pathname pathspec))
	 (compile-pathname (compile-file-pathname pathname))
	 (source-write-time (file-write-date pathname))
	 (target-write-time (and (probe-file compile-pathname)
				 (file-write-date compile-pathname))))
    (when (or (not target-write-time)
	      (<= target-write-time source-write-time))
      (compile-file pathname))
    (load compile-pathname)))
