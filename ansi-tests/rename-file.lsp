;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jan  8 06:22:53 2004
;;;; Contains: Tests for RENAME-FILE

(in-package :cl-test)

(deftest rename-file.1
  (let ((pn1 #p"file-to-be-renamed.txt")
	(pn2 #p"file-that-was-renamed.txt"))
    (when (probe-file pn1)
      (delete-file pn1))
    (when (probe-file pn2)
      (delete-file pn2))
    (with-open-file (s pn1 :direction :output) (format s "Whatever~%"))
    (let ((results (multiple-value-list (rename-file pn1 pn2))))
      (destructuring-bind (defaulted-new-name old-truename new-truename)
	  results
	  (values
	   (=t (length results) 3)
	   (probe-file pn1)
	   (notnot (probe-file pn2))
	   (notnot (probe-file defaulted-new-name))
	   (probe-file old-truename)
	   (notnot (probe-file new-truename))))))
  t nil t t nil t)

(deftest rename-file.2
  (let ((pn1 "file-to-be-renamed.txt")
	(pn2 "file-that-was-renamed.txt"))
    (when (probe-file pn1)
      (delete-file pn1))
    (when (probe-file pn2)
      (delete-file pn2))
    (with-open-file (s pn1 :direction :output) (format s "Whatever~%"))
    (let ((results (multiple-value-list (rename-file pn1 pn2))))
      (destructuring-bind (defaulted-new-name old-truename new-truename)
	  results
	  (values
	   (=t (length results) 3)
	   (probe-file pn1)
	   (notnot (probe-file pn2))
	   (notnot (probe-file defaulted-new-name))
	   (probe-file old-truename)
	   (notnot (probe-file new-truename))))))
  t nil t t nil t)

 (deftest rename-file.3
  (let* ((pn1 (make-pathname :name "file-to-be-renamed"
			     :type "txt"
			     :version :newest
			     :defaults *default-pathname-defaults*))
	 (pn2 (make-pathname :name "file-that-was-renamed"))
	 (pn3 (make-pathname :name "file-that-was-renamed"
			     :defaults pn1)))
    (when (probe-file pn1)
      (delete-file pn1))
    (when (probe-file pn3)
      (delete-file pn3))
    (with-open-file (s pn1 :direction :output) (format s "Whatever~%"))
    (let ((results (multiple-value-list (rename-file pn1 pn2))))
      (destructuring-bind (defaulted-new-name old-truename new-truename)
	  results
	  (values
	   (equalpt (pathname-type pn1)
		    (pathname-type defaulted-new-name))
	   (=t (length results) 3)
	   (probe-file pn1)
	   (notnot (probe-file pn3))
	   (notnot (probe-file defaulted-new-name))
	   (probe-file old-truename)
	   (notnot (probe-file new-truename))))))
  t t nil t t nil t)
    