;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:06:03 1998
;;;; Contains: Package test code, part 13

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; in-package

(deftest in-package-1
    (let ((*package* *package*))
      (declare (special *package*))
      (let ((p2 (in-package "A")))
	(and (eq p2 (find-package "A"))
	     (eq *package* p2))))
  t)

(deftest in-package-2
    (let ((*package* *package*))
      (declare (special *package*))
      (let ((p2 (in-package |A|)))
	(and (eq p2 (find-package "A"))
	     (eq *package* p2))))
  t)

(deftest in-package-3
    (let ((*package* *package*))
      (declare (special *package*))
      (let ((p2 (in-package :|A|)))
	(and (eq p2 (find-package "A"))
	     (eq *package* p2))))
  t)

(deftest in-package-4
    (let ((*package* *package*))
      (declare (special *package*))
      (handler-case
	  (let ((p2 (in-package #\A)))
	    (and (eq p2 (find-package "A"))
		 (eq *package* p2)))
	(error (c) c)))
  t)

(deftest in-package-5
    (let ((*package* *package*))
      (declare (special *package*))
      (ignore-errors (delete-package "H"))
      (handler-case
	  (in-package "H")
	(package-error () 'package-error)
	(error (c) c)))
  package-error)
