;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 15 07:46:22 2004
;;;; Contains: Tests for PATHNAME-MATCH-P

(in-package :cl-test)

(compile-and-load "pathnames-aux.lsp")

;;; Much of the behavior cannot be tested portably.

(deftest pathname-match-p.1
  (let ((pn1 (make-pathname :name :wild))
	(pn2 (make-pathname :name "foo")))
    (pathname-match-p pn1 pn2))
  nil)

(deftest pathname-match-p.2
  (let ((pn1 (make-pathname :type :wild))
	(pn2 (make-pathname :type "txt")))
    (pathname-match-p pn1 pn2))
  nil)

(deftest pathname-match-p.3
  (let ((pn1 (make-pathname :directory '(:absolute :wild)))
	(pn2 (make-pathname :directory '(:absolute))))
    (pathname-match-p pn1 pn2))
  nil)

(deftest pathname-match-p.4
  (let ((pn1 (make-pathname :directory '(:relative :wild)))
	(pn2 (make-pathname :directory '(:relative))))
    (pathname-match-p pn1 pn2))
  nil)

(deftest pathname-match-p.5
  (let ((pn1 (make-pathname :directory '(:relative :wild)))
	(pn2 (make-pathname :directory nil)))
    (and (wild-pathname-p pn1)
	 (not (pathname-directory pn2))
	 (not (pathname-match-p pn1 pn2))))
  nil)

(deftest pathname-match-p.6
  (let ((pn1 (make-pathname :version :wild))
	(pn2 (make-pathname)))
    (and (wild-pathname-p pn1)
	 (not (pathname-version pn2))
	 (not (pathname-match-p pn1 pn2))))
  nil)

;;; Here are error tests

(deftest pathname-match-p.error.1
  (signals-error (pathname-match-p) program-error)
  t)

(deftest pathname-match-p.error.2
  (signals-error (pathname-match-p #p"") program-error)
  t)

(deftest pathname-match-p.error.3
  (signals-error (pathname-match-p #p"" #p"" nil) program-error)
  t)

(deftest pathname-match-p.error.4
  (loop for x in *mini-universe*
	unless (or (could-be-pathname-designator x)
		   (eval `(signals-error (pathname-match-p ',x #p"")
					 type-error)))
	collect x)
  nil)

(deftest pathname-match-p.error.5
  (loop for x in *mini-universe*
	unless (or (could-be-pathname-designator x)
		   (eval `(signals-error (pathname-match-p ',x #p"")
					 type-error
					 :safety 0)))
	collect x)
  nil)

(deftest pathname-match-p.error.6
  (loop for x in *mini-universe*
	unless (or (could-be-pathname-designator x)
		   (eval `(signals-error (pathname-match-p #p"" ',x)
					 type-error)))
	collect x)
  nil)

(deftest pathname-match-p.error.7
  (loop for x in *mini-universe*
	unless (or (could-be-pathname-designator x)
		   (eval `(signals-error (pathname-match-p #p"" ',x)
					 type-error
					 :safety 0)))
	collect x)
  nil)






