;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Sep  9 07:02:00 2003
;;;; Contains: Tests of LOGBITP

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")

(deftest logbitp.error.1
  (classify-error (logbitp))
  program-error)

(deftest logbitp.error.2
  (classify-error (logbitp 0))
  program-error)

(deftest logbitp.error.3
  (classify-error (logbitp 0 0 0))
  program-error)

(deftest logbitp.error.4
  (classify-error (logbitp -1 0))
  type-error)

(deftest logbitp.error.5
  (loop for x in *mini-universe*
	unless (or (integerp x)
		   (eq (eval `(classify-error (logbitp 0 ',x))) 'type-error))
	collect x)
  nil)

(deftest logbitp.1
  (loop for x in *integers*
	unless (if (logbitp 0 x) (oddp x) (evenp x))
	collect x)
  nil)

(deftest logbitp.2
  (loop for len from 0 to 300
	for i = (ash 1 len)
	always (and (logbitp len i)
		    (loop for j from 0 to 300
			  always (or (eql j len)
				     (not (logbitp j i))))))
  t)

(deftest logbitp.3
  (logbitp most-positive-fixnum 0)
  0)
	
(deftest logbitp.4
  (notnot-mv (logbitp most-positive-fixnum -1))
  t)

(deftest logbitp.5
  (logbitp (1+ most-positive-fixnum) 0)
  0)
	
(deftest logbitp.6
  (notnot-mv (logbitp (1+ most-positive-fixnum) -1))
  t)

(deftest logbitp.order.1
  (let ((i 0) a b)
    (values
     (logbitp (progn (setf a (incf i)) 2)
	      (progn (setf b (incf i)) #b111010))
     i a b))
  nil 2 1 2)




	
	
