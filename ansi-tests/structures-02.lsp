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
    (let ((s (make-s-2)))
      (and
       (eql (s-2-f1 s) 0)
       (eq  (s-2-f2 s) 'a)
       (eql (s-2-f3 s) 1.21)
       (eql (s-2-f4 s) #\d)
       (equal (s-2-f5 s) '(a b))
       (eql (s-2-f6 s) *s-2-f6-counter*)))
  t)

;; Two successive invocations of make-s-2 return different objects
(deftest structure-2-2
   (eq (s-2-f5 (make-s-2))
       (s-2-f5 (make-s-2)))
  nil)
    

  



