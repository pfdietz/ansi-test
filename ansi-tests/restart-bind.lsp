;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Mar 21 22:28:53 2003
;;;; Contains: Tests for RESTART-BIND

(in-package :cl-test)

(deftest restart-bind.1
  (restart-bind () nil)
  nil)

(deftest restart-bind.2
  (restart-bind () (values)))
  
(deftest restart-bind.3
  (restart-bind () (values 'a 'b 'c 'd 'e 'f))
  a b c d e f)

(deftest restart-bind.4
  (block nil
    (restart-bind () (return 'good) 'bad))
  good)

(deftest restart-bind.5
  (block done
    (tagbody
     (restart-bind () (go 10) (return-from done 'bad))
     10
     (return-from done 'good)))
  good)




