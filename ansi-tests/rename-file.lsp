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
	   (list (notnot (pathnamep defaulted-new-name))
		 (notnot (pathnamep old-truename))
		 (notnot (pathnamep new-truename))
		 (typep old-truename 'logical-pathname)
		 (typep new-truename 'logical-pathname))
	   (notnot (probe-file defaulted-new-name))
	   (probe-file old-truename)
	   (notnot (probe-file new-truename))))))
  t nil t (t t t nil nil) t nil t)

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
	   (list (notnot (pathnamep defaulted-new-name))
		 (notnot (pathnamep old-truename))
		 (notnot (pathnamep new-truename))
		 (typep old-truename 'logical-pathname)
		 (typep new-truename 'logical-pathname))
	   (notnot (probe-file defaulted-new-name))
	   (probe-file old-truename)
	   (notnot (probe-file new-truename))))))
  t nil t (t t t nil nil) t nil t)

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
	   (list (notnot (pathnamep defaulted-new-name))
		 (notnot (pathnamep old-truename))
		 (notnot (pathnamep new-truename))
		 (typep old-truename 'logical-pathname)
		 (typep new-truename 'logical-pathname))
	   (notnot (probe-file defaulted-new-name))
	   (probe-file old-truename)
	   (notnot (probe-file new-truename))))))
  t t nil t (t t t nil nil) t nil t)

(deftest rename-file.4
  (let ((pn1 "file-to-be-renamed.txt")
	(pn2 "file-that-was-renamed.txt"))
    (when (probe-file pn1)
      (delete-file pn1))
    (when (probe-file pn2)
      (delete-file pn2))
    (let ((s (open pn1 :direction :output)))
      (format s "Whatever~%")
      (close s)
      (let ((results (multiple-value-list (rename-file s pn2))))
	(destructuring-bind (defaulted-new-name old-truename new-truename)
	    results
	  (values
	   (=t (length results) 3)
	   (probe-file pn1)
	   (notnot (probe-file pn2))
	   (list (notnot (pathnamep defaulted-new-name))
		 (notnot (pathnamep old-truename))
		 (notnot (pathnamep new-truename))
		 (typep old-truename 'logical-pathname)
		 (typep new-truename 'logical-pathname))
	   (notnot (probe-file defaulted-new-name))
	   (probe-file old-truename)
	   (notnot (probe-file new-truename)))))))
  t nil t (t t t nil nil) t nil t)

(deftest rename-file.5
  (let ((pn1 "CLTEST:file-to-be-renamed.txt")
	(pn2 "CLTEST:file-that-was-renamed.txt"))
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
	   (list (notnot (pathnamep defaulted-new-name))
		 (notnot (pathnamep old-truename))
		 (notnot (pathnamep new-truename))
		 (typep old-truename 'logical-pathname)
		 (typep new-truename 'logical-pathname))
	   (notnot (probe-file defaulted-new-name))
	   (probe-file old-truename)
	   (notnot (probe-file new-truename))
	   (notnot (typep defaulted-new-name 'logical-pathname))
	   ))))
  t nil t (t t t nil nil) t nil t t)

;;;

(deftest rename-file.error.1
  (classify-error (rename-file))
  program-error)

