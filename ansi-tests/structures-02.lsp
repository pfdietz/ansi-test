;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun May  3 22:46:54 1998
;;;; Contains: Test code for structures, part 02

(in-package :cl-test)
(declaim (optimize (safety 3)))

;; Test initializers for fields

(defvar *s-2-f6-counter* 0)

(defstruct s-2
  (f1 0)
  (f2 'a)
  (f3 1.21)
  (f4 #\d)
  (f5 (list 'a 'b))
  (f6 (incf *s-2-f6-counter*)))

;; Standard structure tests


;; Fields have appropriate values
(deftest structure-2-1
  (let ((*s-2-f6-counter* 0))
    (let ((s (make-s-2)))
      (and
       (eqlt (s-2-f1 s) 0)
       (eqt  (s-2-f2 s) 'a)
       (= (s-2-f3 s) 1.21)
       (eqlt (s-2-f4 s) #\d)
       (equalt (s-2-f5 s) '(a b))
       (eqlt (s-2-f6 s) *s-2-f6-counter*)
       (eqlt *s-2-f6-counter* 1))))
  t)

;; Two successive invocations of make-s-2 return different objects
(deftest structure-2-2
  (let ((*s-2-f6-counter* 0))
    (eqt (s-2-f5 (make-s-2))
	 (s-2-f5 (make-s-2))))
  nil)

;; Creation with various fields does the right thing
(deftest structure-2-3
  (let* ((*s-2-f6-counter* 0)
	 (s (make-s-2 :f1 17)))
    (and
     (eqlt (s-2-f1 s) 17)
     (eqt  (s-2-f2 s) 'a)
     (= (s-2-f3 s) 1.21)
     (eqlt (s-2-f4 s) #\d)
     (equalt (s-2-f5 s) '(a b))
     (eqlt (s-2-f6 s) *s-2-f6-counter*)
     (eqlt *s-2-f6-counter* 1)))
  t)

(deftest structure-2-4
  (let* ((*s-2-f6-counter* 0)
	 (s (make-s-2 :f2 'z)))
    (and
     (eqlt (s-2-f1 s) 0)
     (eqt  (s-2-f2 s) 'z)
     (= (s-2-f3 s) 1.21)
     (eqlt (s-2-f4 s) #\d)
     (equalt (s-2-f5 s) '(a b))
     (eqlt (s-2-f6 s) *s-2-f6-counter*)
     (eqlt *s-2-f6-counter* 1)))
  t)

(deftest structure-2-5
  (let* ((*s-2-f6-counter* 0)
	 (s (make-s-2 :f3 1.0)))
    (and
     (eqlt (s-2-f1 s) 0)
     (eqt  (s-2-f2 s) 'a)
     (= (s-2-f3 s) 1.0)
     (eqlt (s-2-f4 s) #\d)
     (equalt (s-2-f5 s) '(a b))
     (eqlt (s-2-f6 s) *s-2-f6-counter*)
     (eqlt *s-2-f6-counter* 1)))
  t)

(deftest structure-2-6
  (let* ((*s-2-f6-counter* 0)
	 (s (make-s-2 :f4 #\z)))
    (and
     (eqlt (s-2-f1 s) 0)
     (eqt  (s-2-f2 s) 'a)
     (= (s-2-f3 s) 1.21)
     (eqlt (s-2-f4 s) #\z)
     (equalt (s-2-f5 s) '(a b))
     (eqlt (s-2-f6 s) *s-2-f6-counter*)
     (eqlt *s-2-f6-counter* 1)))
  t)

(deftest structure-2-7
  (let* ((*s-2-f6-counter* 0)
	 (s (make-s-2 :f5 '(c d e))))
    (and
     (eqlt (s-2-f1 s) 0)
     (eqt  (s-2-f2 s) 'a)
     (= (s-2-f3 s) 1.21)
     (eqlt (s-2-f4 s) #\d)
     (equalt (s-2-f5 s) '(c d e))
     (eqlt (s-2-f6 s) *s-2-f6-counter*)
     (eqlt *s-2-f6-counter* 1)))
  t)

(deftest structure-2-8
  (let* ((*s-2-f6-counter* 0)
	 (s (make-s-2 :f6 10)))
    (and
     (eqlt (s-2-f1 s) 0)
     (eqt  (s-2-f2 s) 'a)
     (= (s-2-f3 s) 1.21)
     (eqlt (s-2-f4 s) #\d)
     (equalt (s-2-f5 s) '(a b))
     (eqlt (s-2-f6 s) 10)
     (eqlt *s-2-f6-counter* 0)))
  t)

;;; Tests using the defstruct-with-tests infrastructure

(defstruct-with-tests struct-test-03 a b c d)

(defstruct-with-tests (struct-test-04) a b c)

(defstruct-with-tests (struct-test-05 :constructor) a05 b05 c05)
(defstruct-with-tests (struct-test-06 (:constructor)) a06 b06 c06)

(defstruct-with-tests (struct-test-07 :conc-name) a07 b07)
(defstruct-with-tests (struct-test-08 (:conc-name)) a08 b08)
(defstruct-with-tests (struct-test-09 (:conc-name nil)) a09 b09)
(defstruct-with-tests (struct-test-10 (:conc-name "")) a10 b10)
(defstruct-with-tests (struct-test-11 (:conc-name "BLAH-")) a11 b11)
(defstruct-with-tests (struct-test-12 (:conc-name BLAH-)) a12 b12)
(defstruct-with-tests (struct-test-13 (:conc-name #\X)) foo-a13 foo-b13)

(defstruct-with-tests (struct-test-14 (:predicate)) a14 b14)
(defstruct-with-tests (struct-test-15 (:predicate nil)) a15 b15)
(defstruct-with-tests (struct-test-16 :predicate) a16 b16)
(defstruct-with-tests (struct-test-17
		       (:predicate struct-test-17-alternate-pred))
  a17 b17)

(defstruct-with-tests (struct-test-18 :copier) a18 b18)
(defstruct-with-tests (struct-test-19 (:copier)) a19 b19)
(defstruct-with-tests (struct-test-20 (:copier nil)) a20 b20)
(defstruct-with-tests (struct-test-21 (:copier struct-test-21-alt-copier))
  a21 b21)

(defstruct-with-tests struct-test-22 (a22) (b22))
(defstruct-with-tests struct-test-23 (a23 1) (b23 2))
(defstruct-with-tests struct-test-24
  (a24 1 :type fixnum)
  (b24 2 :type integer))

(defstruct-with-tests struct-test-25)
(defstruct-with-tests struct-test-26
  (a26 nil :read-only nil)
  (b26 'a  :read-only nil))

(defstruct-with-tests struct-test-27
  (a27 1    :read-only t)
  (b27 1.4  :read-only a))

(defstruct-with-tests struct-test-28
  (a28 1    :type integer :read-only t)
  (b28 'xx  :read-only a :type symbol))

(defstruct-with-tests struct-test-29
  a29
  (b29 'xx  :read-only 1)
  c29)

(defstruct-with-tests struct-test-30 #:a30 #:b30)
(defstruct-with-tests #:struct-test-31 a31 b31)

(defpackage struct-test-package (:use))

(defstruct-with-tests struct-test-32
  struct-test-package::a32 struct-test-package::b32)

;;; If the :conc-name option is given no argument or
;;; a nil argument, the accessor names are the same as
;;; slot names.  Note that this is different from prepending
;;; an empty string, since that may get you a name in
;;; a different package.

(defstruct-with-tests (struct-test-33 (:conc-name))
  struct-test-package::a33 struct-test-package::b33)
(defstruct-with-tests (struct-test-34 :conc-name)
  struct-test-package::a34 struct-test-package::b34)
(defstruct-with-tests (struct-test-35 (:conc-name nil))
  struct-test-package::a35 struct-test-package::b35)

(defstruct-with-tests (struct-test-36 (:conc-name ""))
  struct-test-package::st36-a36 struct-test-package::st26-b36)

;;; List structures

(defstruct-with-tests (struct-test-37 (:type list)) a37 b37 c37)
(defstruct-with-tests (struct-test-38 (:type list) :named) a38 b38 c38)
(defstruct-with-tests (struct-test-39 (:predicate nil)
				      (:type list) :named)
  a39 b39 c39)

;;; Vector structures

(defstruct-with-tests (struct-test-40 (:type vector)) a40 b40)
(defstruct-with-tests (struct-test-41 (:type vector) :named) a41 b41)
(defstruct-with-tests (struct-test-42 (:type (vector t))) a42 b42)
(defstruct-with-tests (struct-test-43 (:type (vector t)) :named) a43 b43)




