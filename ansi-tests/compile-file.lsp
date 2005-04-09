;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr  9 08:25:25 2005
;;;; Contains: Tests of COMPILE-FILE

(in-package :cl-test)

(deftest compile-file.1
  (let* ((file "compile-file-test-file.lsp")
	 (target-pathname (compile-file-pathname file))
	 (*compile-print* nil)
	 (*compile-verbose* nil)
	 (actual-warnings-p nil)
	 (actual-style-warnings-p nil))
    (when (probe-file target-pathname)
      (delete-file target-pathname))
    (fmakunbound 'compile-file-test-fun.1)
    (let ((vals (multiple-value-list
		 (handler-bind
		  ((style-warning #'(lambda ()
				      (setf actual-style-warnings-p t)
				      (muffle-warning)))
		   (warning #'(lambda ()
				      (setf actual-warnings-p t)
				      (muffle-warning))))
		 (compile-file file)))))
      (assert (= (length vals) 3))
      (destructuring-bind
	  (output-truename warnings-p failure-p)
	  vals
	(values
	 (equalpt target-pathname output-truename)
	 (if warnings-p actual-style-warnings-p 
	   (not actual-style-warnings-p))
	 failure-p
	 (fboundp 'compile-file-test-fun.1)))))
  t t nil nil)

