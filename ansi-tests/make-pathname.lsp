;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 29 05:54:30 2003
;;;; Contains: Tests of MAKE-PATHNAME

(in-package :cl-test)

(defun make-pathname-test
  (&rest args &key (defaults nil)
	 (host (if defaults (pathname-host defaults)
		 (pathname-host *default-pathname-defaults*)))
	 (device (if defaults (pathname-device defaults) nil))
	 (directory (if defaults (pathname-directory defaults) nil))
	 (name (if defaults (pathname-name defaults) nil))
	 (type (if defaults (pathname-type defaults) nil))
	 (version (if defaults (pathname-version defaults) nil))
	 case)
  (declare (ignorable case))
  (let* ((vals (multiple-value-list (apply #'make-pathname args)))
	 (pn (first vals)))
    (and (= (length vals) 1)
	 (typep pn 'pathname)
	 (equalp (pathname-host pn) host)
	 (equalp (pathname-device pn) device)
	 ;; (equalp (pathname-directory pn) directory)
	 (let ((pnd (pathname-directory pn)))
	   (if (eq directory :wild)
	       (member pnd '((:absolute :wild-inferiors)
			     (:absolute :wild))
		       :test #'equal)
	     (equalp pnd directory)))	     
	 (equalp (pathname-name pn) name)
	 (equalp (pathname-type pn) type)
	 (equalp (pathname-version pn) version)
	 t)))
  
  

(deftest make-pathname.1
  (make-pathname-test)
  t)

(deftest make-pathname.2
  (make-pathname-test :name "foo")
  t)

(deftest make-pathname.3
  (make-pathname-test :name "foo" :type "txt")
  t)

(deftest make-pathname.4
  (make-pathname-test :type "lsp")
  t)

(deftest make-pathname.5
  (make-pathname-test :directory :wild)
  t)

(deftest make-pathname.6
  (make-pathname-test :name :wild)
  t)

(deftest make-pathname.7
  (make-pathname-test :type :wild)
  t)

(deftest make-pathname.8
  (make-pathname-test :version :wild)
  t)
