;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jan  1 19:19:42 2005
;;;; Contains: Tests of READTABLEP

(in-package :cl-test)

(deftest readtablep.1
  (notnot-mv (readtablep *readtable*))
  t)

(deftest readtablep.2
  (loop for x in *universe*
	unless (if (readtablep x) (typep x 'readtable)
		 (not (typep x 'readtable)))
	collect x)
  nil)

(deftest readtablep.3
  (notnot-mv (readtablep (copy-readtable)))
  t)

;;; Error tests

(deftest readtablep.error.1
  (signals-error (readtablep) program-error)
  t)

(deftest readtablep.error.2
  (signals-error (readtablep *readtable* nil) program-error)
  t)

(deftest readtablep.error.3
  (signals-error (readtablep *readtable* nil t t t t) program-error)
  t)



