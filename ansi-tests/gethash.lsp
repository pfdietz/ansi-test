;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 06:05:21 2003
;;;; Contains: Tests of GETHASH

(in-package :cl-test)

;;; Most testing of GETHASH is in test-hash-table-1 in hash-table-aux.lsp

(deftest gethash.1
  (gethash 'x (make-hash-table) 'y)
  y nil)

(deftest gethash.2
  (gethash nil (make-hash-table) 'a)
  a nil)

(deftest gethash.3
  (gethash nil (make-hash-table) 'a)
  a nil)

(deftest gethash.4
  (multiple-value-bind (value present)
      (gethash 'a (let ((table (make-hash-table)))
		    (setf (gethash 'a table) 'b)
		    table))
    (values value (notnot present)))
  b t)

(deftest gethash.5
  (let ((table (make-hash-table))
	(i 0))
    (values
     (setf (gethash 'x table (incf i)) 'y)
     i
     (gethash 'x table)))
  y 1 y)

(deftest gethash.order.1
  (let ((i 0) x y
	(table (make-hash-table)))
    (setf (gethash 'a table) 'b)
    (values
     (gethash (progn (setf x (incf i)) 'a)
	      (progn (setf y (incf i)) table))
     i x y))
  b 2 1 2)

(deftest gethash.order.2
  (let ((i 0) x y z
	(table (make-hash-table)))
    (setf (gethash 'a table) 'b)
    (values
     (gethash (progn (setf x (incf i)) 'a)
	      (progn (setf y (incf i)) table)
	      (progn (setf z (incf i)) 'missing))
     i x y z))
  b 3 1 2 3)

(deftest gethash.order.3
  (let ((i 0) x y
	(table (make-hash-table)))
    (values
      (setf (gethash (progn (setf x (incf i)) 'a)
		     (progn (setf y (incf i)) table))
	    'b)
      i x y
      (gethash 'a table)))
  b 2 1 2 b)

(deftest gethash.order.4
  (let ((i 0) x y z
	(table (make-hash-table)))
    (values
      (setf (gethash (progn (setf x (incf i)) 'a)
		     (progn (setf y (incf i)) table)
		     (setf z (incf i)))
	    'b)
      i x y z
      (gethash 'a table)))
  b 3 1 2 3 b)

;;;; Error tests

(deftest gethash.error.1
  (signals-error (gethash) program-error)
  t)

(deftest gethash.error.2
  (signals-error (gethash 'foo) program-error)
  t)

(deftest gethash.error.3
  (signals-error (gethash 'foo (make-hash-table) nil nil) program-error)
  t)
