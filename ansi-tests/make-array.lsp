;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Sep 20 06:47:37 2002
;;;; Contains: Tests for MAKE-ARRAY

(in-package :cl-test)

(defun make-array-check-upgrading (type)
  (subtypep type (array-element-type (make-array 0 :element-type type))))

