;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 07:59:10 1998
;;;; Contains: Package test code, part 04

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; intern

(deftest intern-1
    (let ((p (make-package "TEMP1")))
      (multiple-value-bind (sym1 status1)
	  (find-symbol "FOO" p)
	(intern "FOO" p)
	(multiple-value-bind (sym2 status2)
	    (find-symbol "FOO" p)
	    (and (null sym1)
		 (null status1)
		 (string= (symbol-name sym2) "FOO")
		 (eqt (symbol-package sym2) p)
		 (eqt status2 :internal)
		 (progn (delete-package p) t)))))
  t)

(deftest intern-2
    (let ((p (make-package "TEMP1")))
      (multiple-value-bind (sym1 status1)
	  (find-symbol "FOO" "TEMP1")
	(intern "FOO" "TEMP1")
	(multiple-value-bind (sym2 status2)
	    (find-symbol "FOO" p)
	    (and (null sym1)
		 (null status1)
		 (string= (symbol-name sym2) "FOO")
		 (eqt (symbol-package sym2) p)
		 (eqt status2 :internal)
		 (progn (delete-package p) t)))))
  t)
