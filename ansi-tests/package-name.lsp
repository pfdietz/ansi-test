;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 21 17:48:05 2004
;;;; Contains: Tests of PACKAGE-NAME

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package-name

(deftest package-name.1
  (let () (ignore-errors (package-name "A")))
  "A")

(deftest package-name.2
  (let () (ignore-errors (package-name #\A)))
  "A")

(deftest package-name.3
  (let () (ignore-errors (package-name "Q")))
  "A")

(deftest package-name.4
  (let () (ignore-errors (package-name #\Q)))
  "A")

(deftest package-name.5
  (handler-case
   (locally (declare (optimize safety))
	    (eval '(package-name "NOT-THERE"))
	    nil)
   (type-error () t)
   (package-error () t))
  t)

(deftest package-name.6
  (handler-case
   (locally (declare (optimize safety))
	    (eval '(package-name #\*))
	    nil)
   (type-error () t)
   (package-error () t))
  t)

(deftest package-name.6a
  (handler-case
   (locally (declare (optimize safety))
	    (eval '(locally (package-name #\*) t))
	    nil)
   (type-error () t)
   (package-error () t))
  t)

(deftest package-name.7
  (package-name "CL")
  #.(string '#:common-lisp))

(deftest package-name.8
  (package-name "COMMON-LISP")
  #.(string '#:common-lisp))

(deftest package-name.9
  (package-name "COMMON-LISP-USER")
  #.(string '#:common-lisp-user))

(deftest package-name.10
  (package-name "CL-USER")
  #.(string '#:common-lisp-user))

(deftest package-name.11
  (package-name "KEYWORD")
  #.(string '#:keyword))

(deftest package-name.12
  (package-name (find-package "CL"))
  #.(string '#:common-lisp))

(deftest package-name.13
  (let* ((p (make-package "TEMP1"))
	 (pname1 (package-name p)))
    (rename-package "TEMP1" "TEMP2")
    (let ((pname2 (package-name p)))
      (safely-delete-package p)
      (list pname1 pname2 (package-name p))))
  ("TEMP1" "TEMP2" nil))

;; (find-package (package-name p)) == p for any package p
(deftest package-name.14
  (loop
   for p in (list-all-packages) count
   (not
    (let ((name (package-name p)))
      (and (stringp name)
	   (eqt (find-package name) p)))))
  0)

;; package-name applied to a package's name
;; should return an equal string
(deftest package-name.15
  (loop
   for p in (list-all-packages) count
   (not (equal (package-name p)
	       (ignore-errors (package-name (package-name p))))))
  0)

(deftest package-name.error.1
  (signals-error (package-name) program-error)
  t)

(deftest package-name.error.2
  (signals-error (package-name "CL" nil) program-error)
  t)

