;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Mar 24 03:39:54 2003
;;;; Contains: Tests of DEFCLASS

(in-package :cl-test)

;;; Error tests (let's get them out of the way)

(deftest defclass.error.1
  (classify-error
   (defclass defclass-error-1 nil
     (foo foo)))
  program-error)

(deftest defclass.error.2
  (classify-error
   (defclass defclass-error-2 nil
     (foo (foo))))
  program-error)

(deftest defclass.error.3
  (classify-error
   (defclass defclass-error-3 nil
     ((foo :initarg f1))
     (:default-initargs :f1 10 :f1 20)))
  program-error)

(deftest defclass.error.4
  (classify-error
   (defclass defclass-error-4 nil
     ((foo :initform 10 :initform 20 :reader defclass-error-4/foo))))
  program-error)

(deftest defclass.error.5
  (classify-error
   (defclass defclass-error-5 nil
     ((foo :initform 10 :initform 10 :reader defclass-error-5/foo))))
  program-error)

(deftest defclass.error.6
  (classify-error
   (defclass defclass-error-6 nil
     ((foo :initarg f1 :type t :type t :reader defclass-error-6/foo))))
  program-error)

(deftest defclass.error.7
  (classify-error
   (defclass defclass-error-7 nil
     ((foo :initarg f1 :documentation "x" :reader defclass-error-7/foo
	   :documentation "x"))))
  program-error)

(deftest defclass.error.8
  (classify-error
   (defclass defclass-error-8 ()
     ((foo #:unknown-slot-option nil))))
  program-error)

(deftest defclass.error.9
  (classify-error
   (defclass defclass-error-9 ()
     (foo bar)
     (#:unknown-class-option nil)))
  program-error)


;;; Now non-error tests

(defclass-with-tests defclass-1 nil nil)
(defclass-with-tests defclass-2 nil (slot1 slot2 slot3))


;;; At end, generate slot tests

(generate-slot-tests) ;; a macro





