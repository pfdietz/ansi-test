;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug 20 22:42:35 2002
;;;; Contains: Tests for COUNT-IF-NOT

(in-package :cl-test)

(deftest count-if-not-list.1
  (count-if-not #'identity '(a b nil c d nil e))
  2)

(deftest count-if-not-list.2
  (count-if-not #'not '(a b nil c d nil e))
  5)

(deftest count-if-not-list.3
  (count-if-not #'(lambda (x) (break)) nil)
  0)

(deftest count-if-not-list.4
  (count-if-not #'identity '(a b nil c d nil e) :key #'identity)
  2)

(deftest count-if-not-list.5
  (count-if-not 'identity '(a b nil c d nil e) :key #'identity)
  2)

(deftest count-if-not-list.6
  (count-if-not #'identity '(a b nil c d nil e) :key 'identity)
  2)

(deftest count-if-not-list.8
  (count-if-not #'identity '(a b nil c d nil e) :key 'not)
  5)

(deftest count-if-not-list.9
  (count-if-not #'oddp '(1 2 3 4 4 1 8 10 1))
  5)

(deftest count-if-not-list.10
  (count-if-not #'oddp '(1 2 3 4 4 1 8 10 1) :key #'1+)
  4)

(deftest count-if-not-list.11
  (let ((c 0))
    (count-if-not #'oddp '(1 2 3 4 4 1 8 10 1)
	      :key #'(lambda (x) (+ x (incf c)))))
  6)

(deftest count-if-not-list.12
  (let ((c 0))
    (count-if-not #'oddp '(0 1 2 3 4 4 1 7 10 1)
	      :from-end t
	      :key #'(lambda (x) (+ x (incf c)))))
  8)

(deftest count-if-not-list.13
  (count-if-not #'(lambda (x) (not (eq x 'a)))
	    '(a b c d a e f a e f f a a) :start 2)
  4)

(deftest count-if-not-list.14
  (count-if-not #'(lambda (x) (not (eq x 'a)))
	    '(a b c d a e f a e f f a a) :end 7)
  2)
  
(deftest count-if-not-list.15
  (count-if-not #'(lambda (x) (not (eq x 'a)))
	    '(a b c d a e f a e f f a a) :end 7
	    :start 2)
  1)
  
(deftest count-if-not-list.16
  (count-if-not #'(lambda (x) (not (eq x 'a)))
	    '(a b c d a e f a e f f a a) :end 7
	    :start 2 :from-end t)
  1)
  

;;; tests on vectors

(deftest count-if-not-vector.1
  (count-if-not #'identity #(a b nil c d nil e))
  2)

(deftest count-if-not-vector.2
  (count-if-not #'not #(a b nil c d nil e))
  5)

(deftest count-if-not-vector.3
  (count-if-not #'(lambda (x) (break)) #())
  0)

(deftest count-if-not-vector.4
  (count-if-not #'not #(a b nil c d nil e) :key #'identity)
  5)

(deftest count-if-not-vector.5
  (count-if-not 'not #(a b nil c d nil e) :key #'identity)
  5)

(deftest count-if-not-vector.6
  (count-if-not #'not #(a b nil c d nil e) :key 'identity)
  5)

(deftest count-if-not-vector.8
  (count-if-not #'not #(a b nil c d nil e) :key 'not)
  2)

(deftest count-if-not-vector.9
  (count-if-not #'oddp #(1 2 3 4 4 1 8 10 1))
  5)

(deftest count-if-not-vector.10
  (count-if-not #'oddp #(1 2 3 4 4 1 8 10 1) :key #'1+)
  4)

(deftest count-if-not-vector.11
  (let ((c 0))
    (count-if-not #'oddp #(1 2 3 4 4 1 8 10 1)
	      :key #'(lambda (x) (+ x (incf c)))))
  6)

(deftest count-if-not-vector.12
  (let ((c 0))
    (count-if-not #'oddp #(0 1 2 3 4 4 1 7 10 1)
	      :from-end t
	      :key #'(lambda (x) (+ x (incf c)))))
  8)

(deftest count-if-not-vector.13
  (count-if-not #'(lambda (x) (not (eq x 'a)))
	    #(a b c d a e f a e f f a a) :start 2)
  4)

(deftest count-if-not-vector.14
  (count-if-not #'(lambda (x) (not (eq x 'a)))
	    #(a b c d a e f a e f f a a) :end 7)
  2)
  
(deftest count-if-not-vector.15
  (count-if-not #'(lambda (x) (not (eq x 'a)))
	    #(a b c d a e f a e f f a a) :end 7
	    :start 2)
  1)
  
(deftest count-if-not-vector.16
  (count-if-not #'(lambda (x) (not (eq x 'a)))
	    #(a b c d a e f a e f f a a) :end 7
	    :start 2 :from-end t)
  1)

;;; Non-simple vectors

(deftest count-if-not-nonsimple-vector.1
  (count-if-not #'identity (make-array 7 :initial-contents '(a b nil c d nil e)
				       :fill-pointer t
				       :adjustable t))
  2)

(deftest count-if-not-nonsimple-vector.2
  (count-if-not #'not (make-array 7 :initial-contents '(a b nil c d nil e)
				  :fill-pointer t
				  :adjustable t))
  5)

(deftest count-if-not-nonsimple-vector.3
  (count-if-not #'(lambda (x) (break)) (make-array 0
						   :fill-pointer t
						   :adjustable t))
  0)

(deftest count-if-not-nonsimple-vector.4
  (count-if-not #'not
	    (make-array 7 :initial-contents '(a b nil c d nil e)
			:fill-pointer t
			:adjustable t)
	    :key #'identity)
  5)

(deftest count-if-not-nonsimple-vector.5
  (count-if-not 'not
	    (make-array 7 :initial-contents '(a b nil c d nil e)
			:fill-pointer t
			:adjustable t)
	    :key #'identity)
  5)

(deftest count-if-not-nonsimple-vector.6
  (count-if-not #'not
	    (make-array 7 :initial-contents '(a b nil c d nil e)
			:fill-pointer t
			:adjustable t)
	    :key 'identity)
  5)

(deftest count-if-not-nonsimple-vector.8
  (count-if-not #'not
	    (make-array 7 :initial-contents '(a b nil c d nil e)
			:fill-pointer t
			:adjustable t)
	    :key 'not)
  2)

(deftest count-if-not-nonsimple-vector.9
  (count-if-not #'oddp (make-array 9 :initial-contents '(1 2 3 4 4 1 8 10 1)
				:fill-pointer t :adjustable t))
  5)

(deftest count-if-not-nonsimple-vector.10
  (count-if-not #'oddp
	    (make-array 9 :initial-contents '(1 2 3 4 4 1 8 10 1)
			:fill-pointer t :adjustable t)
	    :key #'1+)
  4)

(deftest count-if-not-nonsimple-vector.11
  (let ((c 0))
    (count-if-not #'oddp
	      (make-array 9 :initial-contents '(1 2 3 4 4 1 8 10 1)
			  :fill-pointer t :adjustable t)
	      :key #'(lambda (x) (+ x (incf c)))))
  6)

(deftest count-if-not-nonsimple-vector.12
  (let ((c 0))
    (count-if-not #'oddp
	      (make-array 10 :initial-contents '(0 1 2 3 4 4 1 7 10 1)
			  :fill-pointer t :adjustable t)
	      :from-end t
	      :key #'(lambda (x) (+ x (incf c)))))
  8)

(deftest count-if-not-nonsimple-vector.13
  (count-if-not #'(lambda (x) (not (eq x 'a)))
	    (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
			:fill-pointer t :adjustable t)
	    :start 2)
  4)

(deftest count-if-not-nonsimple-vector.14
  (count-if-not #'(lambda (x) (not (eq x 'a)))
	    (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
			:fill-pointer t :adjustable t)
	    :end 7)
  2)
  
(deftest count-if-not-nonsimple-vector.15
  (count-if-not #'(lambda (x) (not (eq x 'a)))
	    (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
			:fill-pointer t :adjustable t)
	    :end 7 :start 2)
  1)
  
(deftest count-if-not-nonsimple-vector.16
  (count-if-not #'(lambda (x) (not (eq x 'a)))
	    (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
			:fill-pointer t :adjustable t)
	    :end 7 :start 2 :from-end t)
  1)

;;; tests on bitstrings

(deftest count-if-not-bitstring.1
  (count-if-not #'oddp #*001011101101)
  5)

(deftest count-if-not-bitstring.2
  (count-if-not #'identity #*001011101101)
  0)

(deftest count-if-not-bitstring.3
  (count-if-not #'(lambda (x) (break)) #*)
  0)

(deftest count-if-not-bitstring.4
  (count-if-not #'identity #*001011101101 :key #'zerop)
  7)

(deftest count-if-not-bitstring.5
  (count-if-not 'not #*001011101101 :key #'zerop)
  5)

(deftest count-if-not-bitstring.6
  (count-if-not #'not #*001011101101 :key 'zerop)
  5)

(deftest count-if-not-bitstring.8
  (count-if-not #'identity #*001011101101 :key 'oddp)
  5)

(deftest count-if-not-bitstring.10
  (count-if-not #'oddp #*001011101101 :key #'1+)
  7)

(deftest count-if-not-bitstring.11
  (let ((c 0))
    (count-if-not #'oddp #*001011101101
		  :key #'(lambda (x) (+ x (incf c)))))
  7)

(deftest count-if-not-bitstring.12
  (let ((c 0))
    (count-if-not #'oddp #*001011101101
	      :from-end t
	      :key #'(lambda (x) (+ x (incf c)))))
  5)

(deftest count-if-not-bitstring.13
  (count-if-not #'zerop #*0111011011100 :start 2)
  7)

(deftest count-if-not-bitstring.14
  (count-if-not #'zerop #*0111011011100 :end 7)
  5)
  
(deftest count-if-not-bitstring.15
  (count-if-not #'zerop #*0111011011100 :end 7 :start 2)
  4)
  
(deftest count-if-not-bitstring.16
  (count-if-not #'zerop #*0111011011100 :end 7 :start 2 :from-end t)
  4)

;;; tests on strings

(deftest count-if-not-string.1
  (count-if-not #'(lambda (x) (eql x #\0)) "001011101101")
  7)

(deftest count-if-not-string.2
  (count-if-not #'identity "001011101101")
  0)

(deftest count-if-not-string.3
  (count-if-not #'(lambda (x) (break)) "")
  0)

(deftest count-if-not-string.4
  (count-if-not #'identity "001011101101" :key #'(lambda (x) (eql x #\0)))
  7)

(deftest count-if-not-string.5
  (count-if-not 'identity "001011101101" :key #'(lambda (x) (eql x #\0)))
  7)

(deftest count-if-not-string.6
  (count-if-not #'(lambda (x) (eql x #\0)) "001011101101" :key 'identity)
  7)

(deftest count-if-not-string.8
  (count-if-not #'identity "001011101101" :key #'(lambda (x) (eql x #\1)))
  5)

(deftest count-if-not-string.11
  (let ((c 0))
    (count-if-not #'oddp "001011101101"
		  :key #'(lambda (x) (+ (if (eql x #\0) 0 1) (incf c)))))
  7)

(deftest count-if-not-string.12
  (let ((c 0))
    (count-if-not #'oddp "001011101101"
		  :from-end t
		  :key #'(lambda (x) (+ (if (eql x #\0) 0 1) (incf c)))))
  5)

(deftest count-if-not-string.13
  (count-if-not #'(lambda (x) (eql x #\0)) "0111011011100" :start 2)
  7)

(deftest count-if-not-string.14
  (count-if-not #'(lambda (x) (eql x #\0)) "0111011011100" :end 7)
  5)
  
(deftest count-if-not-string.15
  (count-if-not #'(lambda (x) (eql x #\0)) "0111011011100" :end 7 :start 2)
  4)
  
(deftest count-if-not-string.16
  (count-if-not #'(lambda (x) (eql x #\0))
		"0111011011100" :end 7 :start 2 :from-end t)
  4)

;;; Error tests

(deftest count-if-not-error.1
  (catch-type-error (count-if-not #'identity 1))
  type-error)

(deftest count-if-not-error.2
  (catch-type-error (count-if-not #'identity 'a))
  type-error)

(deftest count-if-not-error.3
  (catch-type-error (count-if-not #'identity #\a))
  type-error)
