;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Dec 30 19:05:01 2003
;;;; Contains: Tests of LOGICAL-PATHNAME

(in-package :cl-test)

(deftest logical-pathname.1
  (loop for x in *logical-pathnames*
	always (eql x (logical-pathname x)))
  t)


