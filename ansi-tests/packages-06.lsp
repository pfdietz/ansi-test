;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:00:28 1998
;;;; Contains: Package test code, part 06

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rename-package

(deftest rename-package-1
    (block nil
      (ignore-errors (delete-package "TEST1"))
      (let ((p (make-package "TEST1")))
	(unless (packagep p) (return nil))
	(let ((p2 (rename-package "TEST1" "TEST2")))
	  (unless (packagep p2)
	    (ignore-errors (delete-package p))
	    (return p2))
	  (unless (and (eqt p p2)
		       (equal (package-name p2) "TEST2"))
	    (ignore-errors (delete-package p))
	    (ignore-errors (delete-package p2))
	    (return nil))
	  (ignore-errors (delete-package p2))
	  t)))
  t)

(deftest rename-package-2
    (block nil
      (ignore-errors (delete-package "TEST1"))
      (let ((p (make-package "TEST1"))
	    (nicknames (copy-list '("TEST3" "TEST4" "TEST5"))))
	(unless (packagep p) (return nil))
	(let ((p2 (rename-package "TEST1" "TEST2" nicknames)))
	  (unless (packagep p2)
	    (ignore-errors (delete-package p))
	    (return p2))
	  (unless (and (eqt p p2)
		       (equal (package-name p2) "TEST2")
		       (null (set-exclusive-or nicknames
					       (package-nicknames p2)
					       :test #'equal)))
	    (ignore-errors (delete-package p))
	    (ignore-errors (delete-package p2))
	    (return nil))
	  (ignore-errors (delete-package p2))
	  t)))
  t)

(deftest rename-package-3
    (block nil
      (ignore-errors (delete-package "TEST1"))
      (let ((p (make-package "TEST1"))
	    (nicknames (copy-list '(#\M #\N))))
	(unless (packagep p) (return nil))
	(let ((p2 (ignore-errors (rename-package "TEST1" "TEST2" nicknames))))
	  (unless (packagep p2)
	    (ignore-errors (delete-package p))
	    (return p2))
	  (unless (and (eqt p p2)
		       (equal (package-name p2) "TEST2")
		       (equal
			(sort (copy-list (package-nicknames p2))
			      #'string<)
			(sort (mapcar #'(lambda (c)
					  (make-string 1 :initial-element c))
				      nicknames)
			      #'string<)))
	    (ignore-errors (delete-package p))
	    (ignore-errors (delete-package p2))
	    (return nil))
	  (ignore-errors (delete-package p2))
	  t)))
  t)

(deftest rename-package-4
    (block nil
      (ignore-errors (delete-package "G"))
      (let ((p (make-package "G"))
	    (nicknames nil))
	(unless (packagep p) (return nil))
	(let ((p2 (ignore-errors (rename-package #\G "TEST2" nicknames))))
	  (unless (packagep p2)
	    (ignore-errors (delete-package p))
	    (return p2))
	  (unless (and (eqt p p2)
		       (equal (package-name p2) "TEST2")
		       (null (set-exclusive-or nicknames
					       (package-nicknames p2)
					       :test #'equal)))
	    (ignore-errors (delete-package p))
	    (ignore-errors (delete-package p2))
	    (return nil))
	  (ignore-errors (delete-package p2))
	  t)))
  t)

(deftest rename-package-5
    (block nil
      (ignore-errors (delete-package "TEST1"))
      (let ((p (make-package "TEST1"))
	    (nicknames nil))
	(unless (packagep p) (return nil))
	(let ((p2 (ignore-errors (rename-package "TEST1" #\G nicknames))))
	  (unless (packagep p2)
	    (ignore-errors (delete-package p))
	    (return p2))
	  (unless (and (eqt p p2)
		       (equal (package-name p2) "G")
		       (null (set-exclusive-or nicknames
					       (package-nicknames p2)
					       :test #'equal)))
	    (ignore-errors (delete-package p))
	    (ignore-errors (delete-package p2))
	    (return nil))
	  (ignore-errors (delete-package p2))
	  t)))
  t)

(deftest rename-package-6
    (block nil
      (ignore-errors (delete-package '|TEST1|))
      (let ((p (make-package '|TEST1|))
	    (nicknames (copy-list '(|M| |N|))))
	(unless (packagep p) (return nil))
	(let ((p2 (ignore-errors (rename-package
			'|TEST1| '|TEST2| nicknames))))
	  (unless (packagep p2)
	    (ignore-errors (delete-package p))
	    (return p2))
	  (unless (and (eqt p p2)
		       (equal (package-name p2) "TEST2")
		       (equal
			(sort (copy-list (package-nicknames p2))
			      #'string<)
			(sort (mapcar #'symbol-name nicknames)
			      #'string<)))
	    (ignore-errors (delete-package p))
	    (ignore-errors (delete-package p2))
	    (return nil))
	  (ignore-errors (delete-package p2))
	  t)))
  t)
