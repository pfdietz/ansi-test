;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 31 18:17:09 2002
;;;; Contains: Tests for SUBSTITUTE-IF-NOT

(in-package :cl-test)

(deftest substitute-if-not-list.1
  (let ((x '())) (values (substitute-if-not 'b #'null x) x))
  nil nil)

(deftest substitute-if-not-list.2
  (let ((x '(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-not-list.3
  (let ((x '(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count nil) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-not-list.4
  (let ((x '(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count 2) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-not-list.5
  (let ((x '(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count 1) x))
  (b b a c)
  (a b a c))

(deftest substitute-if-not-list.6
  (let ((x '(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count 0) x))
  (a b a c)
  (a b a c))

(deftest substitute-if-not-list.7
  (let ((x '(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count -1) x))
  (a b a c)
  (a b a c))

(deftest substitute-if-not-list.8
  (let ((x '())) (values (substitute-if-not 'b (is-not-eq-p 'a) x :from-end t) x))
  nil nil)

(deftest substitute-if-not-list.9
  (let ((x '(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :from-end t) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-not-list.10
  (let ((x '(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :from-end t :count nil) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-not-list.11
  (let ((x '(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count 2 :from-end t) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-not-list.12
  (let ((x '(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count 1 :from-end t) x))
  (a b b c)
  (a b a c))

(deftest substitute-if-not-list.13
  (let ((x '(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count 0 :from-end t) x))
  (a b a c)
  (a b a c))

(deftest substitute-if-not-list.14
  (let ((x '(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count -1 :from-end t) x))
  (a b a c)
  (a b a c))

(deftest substitute-if-not-list.15
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (let* ((orig '(a a a a a a a a a a))
		     (x (copy-seq orig))
		     (y (substitute-if-not 'x (is-not-eq-p 'a) x :start i :end j)))
		(and (equal orig x)
		     (equal y (nconc (make-list i :initial-element 'a)
				     (make-list (- j i) :initial-element 'x)
				     (make-list (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-if-not-list.16
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (let* ((orig '(a a a a a a a a a a))
		     (x (copy-seq orig))
		     (y (substitute-if-not 'x (is-not-eq-p 'a) x :start i :end j :from-end t)))
		(and (equal orig x)
		     (equal y (nconc (make-list i :initial-element 'a)
				     (make-list (- j i) :initial-element 'x)
				     (make-list (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-if-not-list.17
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (loop for c from 0 to (- j i) always
		    (let* ((orig '(a a a a a a a a a a))
			   (x (copy-seq orig))
			   (y (substitute-if-not 'x (is-not-eq-p 'a) x :start i :end j :count c)))
		      (and (equal orig x)
			   (equal y (nconc (make-list i :initial-element 'a)
					   (make-list c :initial-element 'x)
					   (make-list (- 10 (+ i c)) :initial-element 'a))))))))
  t)

(deftest substitute-if-not-list.18
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (loop for c from 0 to (- j i) always
		    (let* ((orig '(a a a a a a a a a a))
			   (x (copy-seq orig))
			   (y (substitute-if-not 'x (is-not-eq-p 'a) x :start i :end j :count c :from-end t)))
		      (and (equal orig x)
			   (equal y (nconc (make-list (- j c) :initial-element 'a)
					   (make-list c :initial-element 'x)
					   (make-list (- 10 j) :initial-element 'a))))))))
  t)

;;; Tests on vectors

(deftest substitute-if-not-vector.1
  (let ((x #())) (values (substitute-if-not 'b (is-not-eq-p 'a) x) x))
  #() #())

(deftest substitute-if-not-vector.2
  (let ((x #(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-if-not-vector.3
  (let ((x #(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count nil) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-if-not-vector.4
  (let ((x #(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count 2) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-if-not-vector.5
  (let ((x #(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count 1) x))
  #(b b a c)
  #(a b a c))

(deftest substitute-if-not-vector.6
  (let ((x #(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count 0) x))
  #(a b a c)
  #(a b a c))

(deftest substitute-if-not-vector.7
  (let ((x #(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count -1) x))
  #(a b a c)
  #(a b a c))

(deftest substitute-if-not-vector.8
  (let ((x #())) (values (substitute-if-not 'b (is-not-eq-p 'a) x :from-end t) x))
  #() #())

(deftest substitute-if-not-vector.9
  (let ((x #(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :from-end t) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-if-not-vector.10
  (let ((x #(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :from-end t :count nil) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-if-not-vector.11
  (let ((x #(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count 2 :from-end t) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-if-not-vector.12
  (let ((x #(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count 1 :from-end t) x))
  #(a b b c)
  #(a b a c))

(deftest substitute-if-not-vector.13
  (let ((x #(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count 0 :from-end t) x))
  #(a b a c)
  #(a b a c))

(deftest substitute-if-not-vector.14
  (let ((x #(a b a c))) (values (substitute-if-not 'b (is-not-eq-p 'a) x :count -1 :from-end t) x))
  #(a b a c)
  #(a b a c))

(deftest substitute-if-not-vector.15
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (let* ((orig #(a a a a a a a a a a))
		     (x (copy-seq orig))
		     (y (substitute-if-not 'x (is-not-eq-p 'a) x :start i :end j)))
		(and (equalp orig x)
		     (equalp y (concatenate 'simple-vector
					   (make-array i :initial-element 'a)
					   (make-array (- j i) :initial-element 'x)
					   (make-array (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-if-not-vector.16
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (let* ((orig #(a a a a a a a a a a))
		     (x (copy-seq orig))
		     (y (substitute-if-not 'x (is-not-eq-p 'a) x :start i :end j :from-end t)))
		(and (equalp orig x)
		     (equalp y (concatenate 'simple-vector
					   (make-array i :initial-element 'a)
					   (make-array (- j i) :initial-element 'x)
					   (make-array (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-if-not-vector.17
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (loop for c from 0 to (- j i) always
		    (let* ((orig #(a a a a a a a a a a))
			   (x (copy-seq orig))
			   (y (substitute-if-not 'x (is-not-eq-p 'a) x :start i :end j :count c)))
		      (and (equalp orig x)
			   (equalp y (concatenate 'simple-vector
						 (make-array i :initial-element 'a)
						 (make-array c :initial-element 'x)
						 (make-array (- 10 (+ i c)) :initial-element 'a))))))))
  t)

(deftest substitute-if-not-vector.18
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (loop for c from 0 to (- j i) always
		    (let* ((orig #(a a a a a a a a a a))
			   (x (copy-seq orig))
			   (y (substitute-if-not 'x (is-not-eq-p 'a) x :start i :end j :count c :from-end t)))
		      (and (equalp orig x)
			   (equalp y (concatenate 'simple-vector
						 (make-array (- j c) :initial-element 'a)
						 (make-array c :initial-element 'x)
						 (make-array (- 10 j) :initial-element 'a))))))))
  t)

;;; Tests on strings

(deftest substitute-if-not-string.1
  (let ((x "")) (values (substitute-if-not #\b (is-not-eq-p #\a) x) x))
  "" "")

(deftest substitute-if-not-string.2
  (let ((x "abac")) (values (substitute-if-not #\b (is-not-eq-p #\a) x) x))
  "bbbc"
  "abac")

(deftest substitute-if-not-string.3
  (let ((x "abac")) (values (substitute-if-not #\b (is-not-eq-p #\a) x :count nil) x))
  "bbbc"
  "abac")

(deftest substitute-if-not-string.4
  (let ((x "abac")) (values (substitute-if-not #\b (is-not-eq-p #\a) x :count 2) x))
  "bbbc"
  "abac")

(deftest substitute-if-not-string.5
  (let ((x "abac")) (values (substitute-if-not #\b (is-not-eq-p #\a) x :count 1) x))
  "bbac"
  "abac")

(deftest substitute-if-not-string.6
  (let ((x "abac")) (values (substitute-if-not #\b (is-not-eq-p #\a) x :count 0) x))
  "abac"
  "abac")

(deftest substitute-if-not-string.7
  (let ((x "abac")) (values (substitute-if-not #\b (is-not-eq-p #\a) x :count -1) x))
  "abac"
  "abac")

(deftest substitute-if-not-string.8
  (let ((x "")) (values (substitute-if-not #\b (is-not-eq-p #\a) x :from-end t) x))
  "" "")

(deftest substitute-if-not-string.9
  (let ((x "abac")) (values (substitute-if-not #\b (is-not-eq-p #\a) x :from-end t) x))
  "bbbc"
  "abac")

(deftest substitute-if-not-string.10
  (let ((x "abac")) (values (substitute-if-not #\b (is-not-eq-p #\a) x :from-end t :count nil) x))
  "bbbc"
  "abac")

(deftest substitute-if-not-string.11
  (let ((x "abac")) (values (substitute-if-not #\b (is-not-eq-p #\a) x :count 2 :from-end t) x))
  "bbbc"
  "abac")

(deftest substitute-if-not-string.12
  (let ((x "abac")) (values (substitute-if-not #\b (is-not-eq-p #\a) x :count 1 :from-end t) x))
  "abbc"
  "abac")

(deftest substitute-if-not-string.13
  (let ((x "abac")) (values (substitute-if-not #\b (is-not-eq-p #\a) x :count 0 :from-end t) x))
  "abac"
  "abac")

(deftest substitute-if-not-string.14
  (let ((x "abac")) (values (substitute-if-not #\b (is-not-eq-p #\a) x :count -1 :from-end t) x))
  "abac"
  "abac")

(deftest substitute-if-not-string.15
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (let* ((orig "aaaaaaaaaa")
		     (x (copy-seq orig))
		     (y (substitute-if-not #\x (is-not-eq-p #\a) x :start i :end j)))
		(and (equalp orig x)
		     (equalp y (concatenate 'simple-string
					   (make-array i :initial-element #\a)
					   (make-array (- j i) :initial-element #\x)
					   (make-array (- 10 j) :initial-element #\a)))))))
  t)

(deftest substitute-if-not-string.16
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (let* ((orig "aaaaaaaaaa")
		     (x (copy-seq orig))
		     (y (substitute-if-not #\x (is-not-eq-p #\a) x :start i :end j :from-end t)))
		(and (equalp orig x)
		     (equalp y (concatenate 'simple-string
					   (make-array i :initial-element #\a)
					   (make-array (- j i) :initial-element #\x)
					   (make-array (- 10 j) :initial-element #\a)))))))
  t)

(deftest substitute-if-not-string.17
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (loop for c from 0 to (- j i) always
		    (let* ((orig "aaaaaaaaaa")
			   (x (copy-seq orig))
			   (y (substitute-if-not #\x (is-not-eq-p #\a) x :start i :end j :count c)))
		      (and (equalp orig x)
			   (equalp y (concatenate 'simple-string
						 (make-array i :initial-element #\a)
						 (make-array c :initial-element #\x)
						 (make-array (- 10 (+ i c)) :initial-element #\a))))))))
  t)

(deftest substitute-if-not-string.18
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (loop for c from 0 to (- j i) always
		    (let* ((orig "aaaaaaaaaa")
			   (x (copy-seq orig))
			   (y (substitute-if-not #\x (is-not-eq-p #\a) x :start i :end j :count c :from-end t)))
		      (and (equalp orig x)
			   (equalp y (concatenate 'simple-string
						 (make-array (- j c) :initial-element #\a)
						 (make-array c :initial-element #\x)
						 (make-array (- 10 j) :initial-element #\a))))))))
  t)

;;; Tests on bitstrings

(deftest substitute-if-not-bitstring.1
  (let* ((orig #*)
	 (x (copy-seq orig))
	 (result (substitute-if-not 0 (is-not-eq-p 1) x)))
    (and (equalp orig x)
	 result))
  #*)

(deftest substitute-if-not-bitstring.2
  (let* ((orig #*)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (complement #'zerop) x)))
    (and (equalp orig x)
	 result))
  #*)

(deftest substitute-if-not-bitstring.3
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 0 (is-not-eq-p 1) x)))
    (and (equalp orig x)
	 result))
  #*000000)

(deftest substitute-if-not-bitstring.4
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (complement #'zerop) x)))
    (and (equalp orig x)
	 result))
  #*111111)

(deftest substitute-if-not-bitstring.5
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (complement #'zerop) x :start 1)))
    (and (equalp orig x)
	 result))
  #*011111)
  
(deftest substitute-if-not-bitstring.6
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 0 (is-not-eq-p 1) x :start 2 :end nil)))
    (and (equalp orig x)
	 result))
  #*010000)

(deftest substitute-if-not-bitstring.7
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (complement #'zerop) x :end 4)))
    (and (equalp orig x)
	 result))
  #*111101)
  
(deftest substitute-if-not-bitstring.8
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 0 (is-not-eq-p 1) x :end nil)))
    (and (equalp orig x)
	 result))
  #*000000)

(deftest substitute-if-not-bitstring.9
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 0 (is-not-eq-p 1) x :end 3)))
    (and (equalp orig x)
	 result))
  #*000101)

(deftest substitute-if-not-bitstring.10
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 0 (is-not-eq-p 1) x :start 2 :end 4)))
    (and (equalp orig x)
	 result))
  #*010001)

(deftest substitute-if-not-bitstring.11
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (complement #'zerop) x :start 2 :end 4)))
    (and (equalp orig x)
	 result))
  #*011101)

(deftest substitute-if-not-bitstring.12
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (complement #'zerop) x :count 1)))
    (and (equalp orig x)
	 result))
  #*110101)

(deftest substitute-if-not-bitstring.13
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (complement #'zerop) x :count 0)))
    (and (equalp orig x)
	 result))
  #*010101)

(deftest substitute-if-not-bitstring.14
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (complement #'zerop) x :count -1)))
    (and (equalp orig x)
	 result))
  #*010101)

(deftest substitute-if-not-bitstring.15
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (complement #'zerop) x :count 1 :from-end t)))
    (and (equalp orig x)
	 result))
  #*010111)

(deftest substitute-if-not-bitstring.16
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (complement #'zerop) x :count 0 :from-end t)))
    (and (equalp orig x)
	 result))
  #*010101)

(deftest substitute-if-not-bitstring.17
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (complement #'zerop) x :count -1 :from-end t)))
    (and (equalp orig x)
	 result))
  #*010101)

(deftest substitute-if-not-bitstring.18
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (complement #'zerop) x :count nil)))
    (and (equalp orig x)
	 result))
  #*111111)

(deftest substitute-if-not-bitstring.19
  (let* ((orig #*010101)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (complement #'zerop) x :count nil :from-end t)))
    (and (equalp orig x)
	 result))
  #*111111)

(deftest substitute-if-not-bitstring.20
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (loop for c from 0 to (- j i) always
		    (let* ((orig #*0000000000)
			   (x (copy-seq orig))
			   (y (substitute-if-not 1 (complement #'zerop) x :start i :end j :count c)))
		      (and (equalp orig x)
			   (equalp y (concatenate
				      'simple-bit-vector
				      (make-list i :initial-element 0)
				      (make-list c :initial-element 1)
				      (make-list (- 10 (+ i c)) :initial-element 0))))))))
  t)

(deftest substitute-if-not-bitstring.21
  (loop for i from 0 to 9 always
	(loop for j from i to 10 always
	      (loop for c from 0 to (- j i) always
		    (let* ((orig #*1111111111)
			   (x (copy-seq orig))
			   (y (substitute-if-not 0 (is-not-eq-p 1) x :start i :end j :count c :from-end t)))
		      (and (equalp orig x)
			   (equalp y (concatenate
				      'simple-bit-vector
				      (make-list (- j c) :initial-element 1)
				      (make-list c :initial-element 0)
				      (make-list (- 10 j) :initial-element 1))))))))
  t)

;;; More tests

(deftest substitute-if-not-list.24
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
	 (x (copy-seq orig))
	 (result (substitute-if-not '(a 10) (is-not-eq-p 'a) x :key #'car)))
    (and (equal orig x)
	 result))
  ((a 10) (b 2) (a 10) (c 4) (d 5) (a 10) (e 7)))

(deftest substitute-if-not-list.25
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
	 (x (copy-seq orig))
	 (result (substitute-if-not '(a 10) (is-not-eq-p 'a) x
				:key #'car :start 1 :end 5)))
    (and (equal orig x)
	 result))
  ((a 1) (b 2) (a 10) (c 4) (d 5) (a 6) (e 7)))

(deftest substitute-if-not-vector.24
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
	 (x (copy-seq orig))
	 (result (substitute-if-not '(a 10) (is-not-eq-p 'a) x :key #'car)))
    (and (equalp orig x)
	 result))
  #((a 10) (b 2) (a 10) (c 4) (d 5) (a 10) (e 7)))
    
(deftest substitute-if-not-vector.25
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
	 (x (copy-seq orig))
	 (result (substitute-if-not '(a 10) (is-not-eq-p 'a) x :key #'car :start 1 :end 5)))
    (and (equalp orig x)
	 result))
  #((a 1) (b 2) (a 10) (c 4) (d 5) (a 6) (e 7)))

(deftest substitute-if-not-string.24
  (let* ((orig "0102342015")
	 (x (copy-seq orig))
	 (result (substitute-if-not #\a (is-not-eq-p #\1) x :key #'nextdigit)))
    (and (equalp orig x)
	 result))
  "a1a2342a15")
    
(deftest substitute-if-not-string.25
  (let* ((orig "0102342015")
	 (x (copy-seq orig))
	 (result (substitute-if-not #\a (is-not-eq-p #\1) x :key #'nextdigit :start 1 :end 6)))
    (and (equalp orig x)
	 result))
  "01a2342015")

(deftest substitute-if-not-bitstring.26
  (let* ((orig #*00111001011010110)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (is-not-eq-p 1) x :key #'1+)))
    (and (equalp orig x)
	 result))
  #*11111111111111111)
    
(deftest substitute-if-not-bitstring.27
  (let* ((orig #*00111001011010110)
	 (x (copy-seq orig))
	 (result (substitute-if-not 1 (is-not-eq-p 1) x :key #'1+ :start 1 :end 10)))
    (and (equalp orig x)
	 result))
  #*01111111111010110)
