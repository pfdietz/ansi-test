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

;;; More to come
