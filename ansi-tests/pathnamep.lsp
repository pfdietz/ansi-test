;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Dec  6 10:26:45 2003
;;;; Contains: Tests of PATHNAMEP

(in-package :cl-test)

(deftest pathnamep.1
  (loop for x in *universe*
	unless (if (pathnamep x) (typep x 'pathname)
		 (not (typep x 'pathname)))
	collect x)
  nil)

(deftest pathnamep.2
  (loop for x in *universe*
	always (eql (length (multiple-value-list (pathnamep x))) 1))
  t)

(deftest pathnamep.3
  (loop for x in *universe*
	always (or (not (typep x 'logical-pathname))
		   (pathnamep x)))
  t)

(deftest pathnamep.error.1
  (classify-error (pathnamep))
  program-error)

(deftest pathnamep.error.2
  (classify-error (pathnamep nil nil))
  program-error)

(deftest pathnamep.error.3
  (classify-error (pathnamep *default-pathname-defaults* nil))
  program-error)
