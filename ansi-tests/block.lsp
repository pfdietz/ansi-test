;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 12:30:46 2002
;;;; Contains: Tests of BLOCK

(in-package :cl-test)

(deftest block.1
  (block foo
    (return-from foo 1))
  1)

(deftest block.2
  (block nil
    (block foo
      (return 'good))
    'bad)
  good)

(deftest block.3
  (block done
    (flet ((%f (x) (return-from done x)))
      (%f 'good))
    'bad)
  good)

(deftest block.4
  (block foo
    (block foo
      (return-from foo 'bad))
    'good)
  good)

(deftest block.5
  (block done
    (flet ((%f (x) (return-from done x)))
      (mapcar #'%f '(good bad bad)))
    'bad)
  good)


      
