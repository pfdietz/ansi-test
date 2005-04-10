;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr  9 08:25:25 2005
;;;; Contains: Tests of COMPILE-FILE

(in-package :cl-test)

(defun compile-file-test (file funname &rest args &key expect-warnings output-file
			       print verbose external-format)
  (declare (ignorable verbose external-format))
  (let* ((target-pathname (or output-file
			      (compile-file-pathname file)))
	 (*compile-print* nil)
	 (*compile-verbose* nil)
	 (actual-warnings-p nil)
	 (actual-style-warnings-p nil))
    (when (probe-file target-pathname)
      (delete-file target-pathname))
    (fmakunbound funname)
    (let* ((str (make-array '(0) :element-type 'character :adjustable t :fill-pointer 0))
	   (vals (multiple-value-list
		  (handler-bind
		   ((style-warning #'(lambda (c)
				       (declare (ignore c))
				       (setf actual-style-warnings-p t)
				       nil))
		    (warning #'(lambda (c)
				 (declare (ignore c))
				 (setf actual-warnings-p t)
				 nil)))
		   (with-output-to-string
		     (*standard-output* str)
		     (apply #'compile-file file :allow-other-keys t args))))))
      (assert (= (length vals) 3))
      (assert (or print verbose (string= str "")) ()
	      "PRINT and VERBOSE are NIL and str is not empty: ~A"
	      str)
      (assert (or (not verbose) (position #\; str)))
      (destructuring-bind
	  (output-truename warnings-p failure-p)
	  vals
	(values
	 (equalpt (namestring (truename target-pathname))
		  (namestring output-truename))
	 (if expect-warnings (list ; (notnot warnings-p)
				   actual-style-warnings-p)
	   (if warnings-p actual-style-warnings-p 
	     (not actual-style-warnings-p)))
	 (notnot failure-p)
	 (notnot (fboundp funname))
	 (progn
	   (load target-pathname)
	   (funcall funname)))))))

(deftest compile-file.1
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1)
  t t nil nil nil)

(deftest compile-file.2
  (compile-file-test "compile-file-test-file-2.lsp" 'compile-file-test-fun.2 :expect-warnings t)
  t (t) nil nil nil)

(deftest compile-file.3
  (let ((*package* (find-package "CL-TEST")))
    (compile-file-test "compile-file-test-file-3.lsp" 'compile-file-test-fun.3))
  t t nil nil nil)

(deftest compile-file.4
  (let ((*package* (find-package "CL-USER")))
    (compile-file-test "compile-file-test-file-3.lsp" 'cl-user::compile-file-test-fun.3))
  t t nil nil nil)

(deftest compile-file.5
  (compile-file-test #p"compile-file-test-file.lsp" 'compile-file-test-fun.1)
  t t nil nil nil)

(deftest compile-file.6
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :output-file "foo.fasl")
  t t nil nil nil)

(deftest compile-file.7
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :external-format :default)
  t t nil nil nil)

(deftest compile-file.8
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :output-file #p"foo.fasl")
  t t nil nil nil)

(deftest compile-file.9
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :print t)
  t t nil nil nil)

(deftest compile-file.10
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :verbose t)
  t t nil nil nil)

(deftest compile-file.11
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :print nil)
  t t nil nil nil)

(deftest compile-file.12
  (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		     :verbose nil)
  t t nil nil nil)

;;; A file stream is a pathname designator
(deftest compile-file.13
  (with-open-file (s "compile-file-test-file.lsp" :direction :input)
		  (compile-file-test s 'compile-file-test-fun.1))
  t t nil nil nil)

(deftest compile-file.14
  (let ((s (open "foo.fasl" :direction :output :if-exists :supersede
		 :if-does-not-exist :create)))
    (close s)
    (compile-file-test "compile-file-test-file.lsp" 'compile-file-test-fun.1
		       :output-file s))
  t t nil nil nil)

;;; Add tests for *compile-file-truename*, *compile-file-pathname*

;;; Error cases

(deftest compile-file.error.1
  (signals-error (compile-file "nonexistent-file-to-compile.lsp") file-error)
  t)

(deftest compile-file.error.2
  (signals-error (compile-file) program-error)
  t)




