;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 12:56:56 2003
;;;; Contains: Tests of DEFTYPE

(in-package :cl-test)

(compile-and-load "types-aux.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; deftype

(deftest deftype.1
  (typep 1 '(even-array integer (10)))
  nil)

(deftest deftype.2
  (typep nil '(even-array t (*)))
  nil)

(deftest deftype.3
  (notnot-mv (typep (make-array '(10)) '(even-array t (*))))
  t)

(deftest deftype.4
  (typep (make-array '(5)) '(even-array t (*)))
  nil)

(deftest deftype.5
  (notnot-mv (typep (make-string 10) '(even-array character (*))))
  t)

(deftest deftype.6
  (notnot-mv
   (typep (make-array '(3 5 6) :element-type '(unsigned-byte 8))
	  '(even-array (unsigned-byte 8))))
  t)


;;; Error tests

(deftest deftype.error.1
  (signals-error (funcall (macro-function 'deftype))
		 program-error)
  t)

(deftest deftype.error.2
  (signals-error (funcall (macro-function 'deftype)
			   '(deftype nonexistent-type () nil))
		 program-error)
  t)

(deftest deftype.error.3
  (signals-error (funcall (macro-function 'deftype)
			   '(deftype nonexistent-type () nil)
			   nil nil)
		 program-error)
  t)

