;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Apr 25 06:59:22 2003
;;;; Contains: Error case tests for DEFCLASS

(in-package :cl-test)

(deftest defclass.error.1
  (classify-error
   (defclass erroneous-class.1 ()
     (a b c d b e)))
  program-error)

(deftest defclass.error.2
  (classify-error
   (defclass erroneous-class.2 ()
     ((s1 :initarg :foo))
     (:default-initargs :foo 1 :foo 2)))
  program-error)

(deftest defclass.error.3
  (classify-error
   (defclass erroneous-class.3 ()
     ((s1 :initform 0 :initform 2))))
  program-error)

(deftest defclass.error.4
  (classify-error
   (defclass erroneous-class.4 ()
     ((s1 :initform 0 :initform 0))))
  program-error)

(deftest defclass.error.5
  (classify-error
   (defclass erroneous-class.5 ()
     ((s1 :type fixnum :type character))))
  program-error)

(deftest defclass.error.6
  (classify-error
   (defclass erroneous-class.6 ()
     ((s1 :type t :type t))))
  program-error)

(deftest defclass.error.7
  (classify-error
   (defclass erroneous-class.7 ()
     ((s1 :documentation "foo" :documentation "bar"))))
  program-error)

(deftest defclass.error.8
  (classify-error
   (defclass erroneous-class.8 ()
     ((s1 :documentation #1="foo" :documentation #1#))))
  program-error)

(deftest defclass.error.9
  (classify-error
   (defclass erroneous-class.9 ()
     ((s1 :allocation :class :allocation :instance))))
  program-error)

(deftest defclass.error.10
  (classify-error
   (defclass erroneous-class.10 ()
     ((s1 :allocation :class :allocation :class))))
  program-error)

(deftest defclass.error.11
  (classify-error
   (defclass erroneous-class.11 ()
     ((s1 :allocation :instance :allocation :instance))))
  program-error)

(deftest defclass.error.12
  (classify-error
   (defclass erroneous-class.12 ()
     ((s1 #.(gensym) nil))))
  program-error)

(deftest defclass.error.13
  (classify-error
   (defclass erroneous-class.13 ()
     (a b c)
     (#.(gensym))))
  program-error)