;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Nov 28 21:18:12 2003
;;;; Contains: Tests of SXHASH

(in-package :cl-test)

(deftest sxhash.1
  (loop for x in *universe*
	for hash-code = (sxhash x)
	unless (typep hash-code '(and unsigned-byte fixnum))
	collect x)
  nil)

(deftest sxhash.2
  (loop for i from 0 below 256
	for c = (code-char i)
	when (and c
		  (not (= (sxhash (string c))
			  (sxhash (string c)))))
	collect c)
  nil)

(deftest sxhash.3
  (=t (sxhash "") (sxhash (copy-seq "")))
  t)

(deftest sxhash.4
  (loop for bv1 in '(#* #*0 #*1 #*01 #*00 #*10 #*11
			#*1100101101100 #*110010101011001011010000111001011)
	for bv2 = (copy-seq bv1)
	for sx1 = (sxhash bv1)
	for sx2 = (sxhash bv2)
	always (and (not (eq bv1 bv2))
		    (equal bv1 bv2)
		    (typep sx1 '(and unsigned-byte fixnum))
		    (typep sx2 '(and unsigned-byte fixnum))
		    (= sx1 sx2)))
  t)

(deftest sxhash.5
  (let ((s1 "abcd")
	(s2 (make-array 10 :element-type 'character
			:initial-contents "abcdefghij"
			:fill-pointer 4)))
    (and (equalt s1 s2)
	 (=t (sxhash s1) (sxhash s2))))
  t)

(deftest sxhash.6
  (let ((s1 #*01101)
	(s2 (make-array 10 :element-type 'bit
			:initial-contents #*0110111101
			:fill-pointer 5)))
    (and (equalt s1 s2)
	 (=t (sxhash s1) (sxhash s2))))
  t)

(deftest sxhash.7
  (let* ((a (make-array 10 :initial-element nil))
	 (sx1 (sxhash a)))
    (setf (aref a 4) 'x)
    (let ((sx2 (sxhash a)))
      (and (typep sx1 '(and unsigned-byte fixnum))
	   (eqlt sx1 sx2))))
  t)

(deftest sxhash.8
  :notes (:nil-vectors-are-strings)
  (eqlt (sxhash (make-array 0 :element-type nil))
	(sxhash ""))
  t)

(deftest sxhash.9
  (let ((s1 (make-array 5 :element-type 'base-char :initial-contents "abcde"))
	(s2 (copy-seq "abcde")))
    (eqlt (sxhash s1) (sxhash s2)))
  t)

(deftest sxhash.10
  (let ((s1 "abcd")
	(s2 (make-array 10 :element-type 'base-char
			:initial-contents "abcdefghij"
			:fill-pointer 4)))
    (and (equalt s1 s2)
	 (=t (sxhash s1) (sxhash s2))))
  t)

(deftest sxhash.11
  (let* ((x (cons 'a 'b))
	 (sx1 (sxhash x))
	 (sx2 (sxhash '(a . b))))
    (setf (car x) 'c)
    (let* ((sx3 (sxhash x))
	   (sx4 (sxhash '(c . b))))
      (and (=t sx1 sx2)
	   (=t sx3 sx4))))
  t)

(deftest sxhash.12
  (let ((x (1+ most-positive-fixnum))
	(y (1+ most-positive-fixnum)))
    (=t (sxhash x) (sxhash y)))
  t)

(deftest sxhash.13
  (let ((sx1 (sxhash (make-symbol "FOO")))
	(sx2 (sxhash (make-symbol "FOO"))))		      
    (and (typep sx1 '(and unsigned-byte fixnum))
	 (eqlt sx1 sx2)))
  t)

;; (deftest sxhash.14
;;  (let ((sx1 (sxhash :foo))
;;	(sx2 (sxhash '#:foo)))
;;    (and (typep sx1 '(and unsigned-byte fixnum))
;;	 (eqlt sx1 sx2)))
;;  t)

(deftest sxhash.15
  (let* ((package-name
	  (loop for i from 0
		for name = (format nil "PACKAGE-~A" i)
		for package = (find-package name)
		unless package do (return name)))
	 (sx1
	  (let* ((package (make-package package-name nil nil))
		 (symbol (intern "FOO" package)))
	    (prog1
	       (sxhash symbol)
	      (delete-package package))))
	 (sx2
	  (let* ((package (make-package package-name nil nil))
		 (symbol (intern "FOO" package)))
	    (prog1
	       (sxhash symbol)
	      (delete-package package)))))
    (assert (typep sx1 '(and unsigned-byte fixnum)))
    (if (= sx1 sx2) :good (list sx1 sx2)))
  :good)

(deftest sxhash.16
  (let ((c1 (list 'a))
	(c2 (list 'a)))
    (setf (cdr c1) c1)
    (setf (cdr c2) c2)
    (let ((sx1 (sxhash c1))
	  (sx2 (sxhash c2)))
      (or (eqlt sx1 sx2) (list sx1 sx2))))
  t)

;;; Since similarity of numbers is 'same type and same mathematical value',
;;; and since sxhash must produce the same value for similar numeric arguments,
;;; (sxhash 0.0) and (sxhash -0.0) must be eql for all float types.

(deftest sxhash.17
  (loop for c1 in '(0.0s0 0.0f0 0.0d0 0.0l0)
	for c2 in '(-0.0s0 -0.0f0 -0.0d0 -0.0l0)
	for sx1 = (sxhash c1)
	for sx2 = (sxhash c2)
	unless (eql sx1 sx2)
	collect (list c1 c2 sx1 sx2))
  nil)

(deftest sxhash.18
  (loop for r1 in '(0.0s0 0.0f0 0.0d0 0.0l0)
	for c1 = (complex r1)
	for r2 in '(-0.0s0 -0.0f0 -0.0d0 -0.0l0)
	for c2 = (complex r2)
	for sx1 = (sxhash c1)
	for sx2 = (sxhash c2)
	unless (eql sx1 sx2)
	collect (list c1 c2 sx1 sx2))
  nil)

(deftest sxhash.19
  (loop for r1 in '(0.0s0 0.0f0 0.0d0 0.0l0)
	for c1 = (complex 0 r1)
	for r2 in '(-0.0s0 -0.0f0 -0.0d0 -0.0l0)
	for c2 = (complex 0 r2)
	for sx1 = (sxhash c1)
	for sx2 = (sxhash c2)
	unless (eql sx1 sx2)
	collect (list c1 c2 sx1 sx2))
  nil)

;;; Error cases

(deftest sxhash.error.1
  (signals-error (sxhash) program-error)
  t)

(deftest sxhash.error.2
  (signals-error (sxhash nil nil) program-error)
  t)
