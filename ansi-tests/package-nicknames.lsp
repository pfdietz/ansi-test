;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 07:51:26 1998
;;;; Contains: Tests of PACKAGE-NICKNAMES

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package-nicknames

(deftest package-nicknames.1
  (let () (ignore-errors (package-nicknames "A")))
  ("Q"))

(deftest package-nicknames.2
  (let () (ignore-errors (package-nicknames #\A)))
  ("Q"))

(deftest package-nicknames.3
  (let () (ignore-errors (package-nicknames ':|A|)))
  ("Q"))

(deftest package-nicknames.4
  (let () (ignore-errors (package-nicknames "B")))
  nil)

(deftest package-nicknames.5
  (let () (ignore-errors (package-nicknames #\B)))
  nil)

(deftest package-nicknames.6
  (let () (ignore-errors (package-nicknames '#:|B|)))
  nil)

(deftest package-nicknames.7
  (let ()
    (ignore-errors
      (subsetp '(#.(string '#:cl))
	       (package-nicknames "COMMON-LISP")
	       :test #'string=)))
  t)

(deftest package-nicknames.8
  (let ()
    (ignore-errors
      (notnot
       (subsetp '(#.(string '#:cl-user))
		(package-nicknames "COMMON-LISP-USER")
		:test #'string=))))
  t)

(deftest package-nicknames.9
  (signals-error (package-nicknames 10) type-error)
  t)

(deftest package-nicknames.9a
  (signals-error (locally (package-nicknames 10) t) type-error)
  t)

(deftest package-nicknames.10
  (let () (ignore-errors (package-nicknames (find-package "A"))))
  ("Q"))

(deftest package-nicknames.11
  (handler-case
   (locally (declare (optimize safety))
	    (eval '(package-nicknames "NOT-A-PACKAGE-NAME"))
	    nil)
   (type-error () t)
   (package-error () t))
  t)

;; (find-package n) == p for each n in (package-nicknames p),
;; for any package p
(deftest package-nicknames.12
  (loop
   for p in (list-all-packages) sum
   (loop
    for nk in (package-nicknames p) count
	 (not
	  (and (stringp nk)
	       (eqt p (find-package nk))))))
  0)

(deftest package-nicknames.error.1
  (signals-error (package-nicknames) program-error)
  t)

(deftest package-nicknames.error.2
  (signals-error (package-nicknames "CL" nil) program-error)
  t)
