;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Dec  6 14:45:16 2003
;;;; Contains: Tests for PATHNAME-NAME

(in-package :cl-test)

(deftest pathname-name.1
  (loop for p in *pathnames*
	for name = (pathname-name p)
	unless (or (stringp name)
		   (member name '(nil :wild :unspecific)))
	collect (list p name))
  nil)

(deftest pathname-name.2
  (loop for p in *pathnames*
	for name = (pathname-name p :case :local)
	unless (or (stringp name)
		   (member name '(nil :wild :unspecific)))
	collect (list p name))
  nil)

(deftest pathname-name.3
  (loop for p in *pathnames*
	for name = (pathname-name p :case :common)
	unless (or (stringp name)
		   (member name '(nil :wild :unspecific)))
	collect (list p name))
  nil)

(deftest pathname-name.4
  (loop for p in *pathnames*
	for name = (pathname-name p :allow-other-keys nil)
	unless (or (stringp name)
		   (member name '(nil :wild :unspecific)))
	collect (list p name))
  nil)

(deftest pathname-name.5
  (loop for p in *pathnames*
	for name = (pathname-name p :foo 'bar :allow-other-keys t)
	unless (or (stringp name)
		   (member name '(nil :wild :unspecific)))
	collect (list p name))
  nil)

(deftest pathname-name.6
  (loop for p in *pathnames*
	for name = (pathname-name p :allow-other-keys t :allow-other-keys nil :foo 'bar)
	unless (or (stringp name)
		   (member name '(nil :wild :unspecific)))
	collect (list p name))
  nil)

;;; section 19.3.2.1
(deftest pathname-name.7
  (loop for p in *logical-pathnames*
	when (eq (pathname-name p) :unspecific)
	collect p)
  nil)

(deftest pathname-name.error.1
  (classify-error (pathname-name))
  program-error)

(deftest pathname-name.error.2
  (loop for x in *mini-universe*
	unless (or (could-be-pathname-designator x)
		   (handler-case (progn (pathname-name x) nil)
				 (type-error () t)
				 (condition () nil)))
	collect x)
  nil)
