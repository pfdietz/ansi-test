;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug 20 08:01:30 2002
;;;; Contains: Tests for COUNT-IF

(in-package :cl-test)

(deftest count-if-list.1
  (count-if #'identity '(a b nil c d nil e))
  5)

(deftest count-if-list.2
  (count-if #'not '(a b nil c d nil e))
  2)

(deftest count-if-list.3
  (count-if #'(lambda (x) (break)) nil)
  0)

(deftest count-if-list.4
  (count-if #'identity '(a b nil c d nil e) :key #'identity)
  5)

(deftest count-if-list.5
  (count-if 'identity '(a b nil c d nil e) :key #'identity)
  5)

(deftest count-if-list.6
  (count-if #'identity '(a b nil c d nil e) :key 'identity)
  5)

(deftest count-if-list.8
  (count-if #'identity '(a b nil c d nil e) :key 'not)
  2)

(deftest count-if-list.9
  (count-if #'evenp '(1 2 3 4 4 1 8 10 1))
  5)

(deftest count-if-list.10
  (count-if #'evenp '(1 2 3 4 4 1 8 10 1) :key #'1+)
  4)

(deftest count-if-list.11
  (let ((c 0))
    (count-if #'evenp '(1 2 3 4 4 1 8 10 1)
	      :key #'(lambda (x) (+ x (incf c)))))
  6)

(deftest count-if-list.12
  (let ((c 0))
    (count-if #'evenp '(0 1 2 3 4 4 1 7 10 1)
	      :from-end t
	      :key #'(lambda (x) (+ x (incf c)))))
  8)

(deftest count-if-list.13
  (count-if #'(lambda (x) (eqt x 'a))
	    '(a b c d a e f a e f f a a) :start 2)
  4)

(deftest count-if-list.14
  (count-if #'(lambda (x) (eqt x 'a))
	    '(a b c d a e f a e f f a a) :end 7)
  2)
  
(deftest count-if-list.15
  (count-if #'(lambda (x) (eqt x 'a))
	    '(a b c d a e f a e f f a a) :end 7
	    :start 2)
  1)
  
(deftest count-if-list.16
  (count-if #'(lambda (x) (eqt x 'a))
	    '(a b c d a e f a e f f a a) :end 7
	    :start 2 :from-end t)
  1)
  

;;; tests on vectors

(deftest count-if-vector.1
  (count-if #'identity #(a b nil c d nil e))
  5)

(deftest count-if-vector.2
  (count-if #'not #(a b nil c d nil e))
  2)

(deftest count-if-vector.3
  (count-if #'(lambda (x) (break)) #())
  0)

(deftest count-if-vector.4
  (count-if #'identity #(a b nil c d nil e) :key #'identity)
  5)

(deftest count-if-vector.5
  (count-if 'identity #(a b nil c d nil e) :key #'identity)
  5)

(deftest count-if-vector.6
  (count-if #'identity #(a b nil c d nil e) :key 'identity)
  5)

(deftest count-if-vector.8
  (count-if #'identity #(a b nil c d nil e) :key 'not)
  2)

(deftest count-if-vector.9
  (count-if #'evenp #(1 2 3 4 4 1 8 10 1))
  5)

(deftest count-if-vector.10
  (count-if #'evenp #(1 2 3 4 4 1 8 10 1) :key #'1+)
  4)

(deftest count-if-vector.11
  (let ((c 0))
    (count-if #'evenp #(1 2 3 4 4 1 8 10 1)
	      :key #'(lambda (x) (+ x (incf c)))))
  6)

(deftest count-if-vector.12
  (let ((c 0))
    (count-if #'evenp #(0 1 2 3 4 4 1 7 10 1)
	      :from-end t
	      :key #'(lambda (x) (+ x (incf c)))))
  8)

(deftest count-if-vector.13
  (count-if #'(lambda (x) (eqt x 'a))
	    #(a b c d a e f a e f f a a) :start 2)
  4)

(deftest count-if-vector.14
  (count-if #'(lambda (x) (eqt x 'a))
	    #(a b c d a e f a e f f a a) :end 7)
  2)
  
(deftest count-if-vector.15
  (count-if #'(lambda (x) (eqt x 'a))
	    #(a b c d a e f a e f f a a) :end 7
	    :start 2)
  1)
  
(deftest count-if-vector.16
  (count-if #'(lambda (x) (eqt x 'a))
	    #(a b c d a e f a e f f a a) :end 7
	    :start 2 :from-end t)
  1)

;;; Non-simple vectors

(deftest count-if-nonsimple-vector.1
  (count-if #'identity (make-array 7 :initial-contents '(a b nil c d nil e)
				   :fill-pointer t
				   :adjustable t))
  5)

(deftest count-if-nonsimple-vector.2
  (count-if #'not (make-array 7 :initial-contents '(a b nil c d nil e)
				   :fill-pointer t
				   :adjustable t))
  2)

(deftest count-if-nonsimple-vector.3
  (count-if #'(lambda (x) (break)) (make-array 0
				   :fill-pointer t
				   :adjustable t))
  0)

(deftest count-if-nonsimple-vector.4
  (count-if #'identity
	    (make-array 7 :initial-contents '(a b nil c d nil e)
			:fill-pointer t
			:adjustable t)
	    :key #'identity)
  5)

(deftest count-if-nonsimple-vector.5
  (count-if 'identity
	    (make-array 7 :initial-contents '(a b nil c d nil e)
			:fill-pointer t
			:adjustable t)
	    :key #'identity)
  5)

(deftest count-if-nonsimple-vector.6
  (count-if #'identity
	    (make-array 7 :initial-contents '(a b nil c d nil e)
			:fill-pointer t
			:adjustable t)
	    :key 'identity)
  5)

(deftest count-if-nonsimple-vector.8
  (count-if #'identity
	    (make-array 7 :initial-contents '(a b nil c d nil e)
			:fill-pointer t
			:adjustable t)
	    :key 'not)
  2)

(deftest count-if-nonsimple-vector.9
  (count-if #'evenp (make-array 9 :initial-contents '(1 2 3 4 4 1 8 10 1)
				:fill-pointer t :adjustable t))
  5)

(deftest count-if-nonsimple-vector.10
  (count-if #'evenp
	    (make-array 9 :initial-contents '(1 2 3 4 4 1 8 10 1)
			:fill-pointer t :adjustable t)
	    :key #'1+)
  4)

(deftest count-if-nonsimple-vector.11
  (let ((c 0))
    (count-if #'evenp
	      (make-array 9 :initial-contents '(1 2 3 4 4 1 8 10 1)
			  :fill-pointer t :adjustable t)
	      :key #'(lambda (x) (+ x (incf c)))))
  6)

(deftest count-if-nonsimple-vector.12
  (let ((c 0))
    (count-if #'evenp
	      (make-array 10 :initial-contents '(0 1 2 3 4 4 1 7 10 1)
			  :fill-pointer t :adjustable t)
	      :from-end t
	      :key #'(lambda (x) (+ x (incf c)))))
  8)

(deftest count-if-nonsimple-vector.13
  (count-if #'(lambda (x) (eqt x 'a))
	    (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
			:fill-pointer t :adjustable t)
	    :start 2)
  4)

(deftest count-if-nonsimple-vector.14
  (count-if #'(lambda (x) (eqt x 'a))
	    (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
			:fill-pointer t :adjustable t)
	    :end 7)
  2)
  
(deftest count-if-nonsimple-vector.15
  (count-if #'(lambda (x) (eqt x 'a))
	    (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
			:fill-pointer t :adjustable t)
	    :end 7 :start 2)
  1)
  
(deftest count-if-nonsimple-vector.16
  (count-if #'(lambda (x) (eqt x 'a))
	    (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
			:fill-pointer t :adjustable t)
	    :end 7 :start 2 :from-end t)
  1)

;;; tests on bitstrings

(deftest count-if-bitstring.1
  (count-if #'evenp #*001011101101)
  5)

(deftest count-if-bitstring.2
  (count-if #'identity #*001011101101)
  12)

(deftest count-if-bitstring.3
  (count-if #'(lambda (x) (break)) #*)
  0)

(deftest count-if-bitstring.4
  (count-if #'identity #*001011101101 :key #'zerop)
  5)

(deftest count-if-bitstring.5
  (count-if 'identity #*001011101101 :key #'zerop)
  5)

(deftest count-if-bitstring.6
  (count-if #'identity #*001011101101 :key 'zerop)
  5)

(deftest count-if-bitstring.8
  (count-if #'identity #*001011101101 :key 'oddp)
  7)

(deftest count-if-bitstring.10
  (count-if #'evenp #*001011101101 :key #'1+)
  7)

(deftest count-if-bitstring.11
  (let ((c 0))
    (count-if #'evenp #*001011101101
	      :key #'(lambda (x) (+ x (incf c)))))
  7)

(deftest count-if-bitstring.12
  (let ((c 0))
    (count-if #'evenp #*001011101101
	      :from-end t
	      :key #'(lambda (x) (+ x (incf c)))))
  5)

(deftest count-if-bitstring.13
  (count-if #'zerop #*0111011011100 :start 2)
  4)

(deftest count-if-bitstring.14
  (count-if #'zerop #*0111011011100 :end 7)
  2)
  
(deftest count-if-bitstring.15
  (count-if #'zerop #*0111011011100 :end 7 :start 2)
  1)
  
(deftest count-if-bitstring.16
  (count-if #'zerop #*0111011011100 :end 7 :start 2 :from-end t)
  1)

;;; tests on strings

(deftest count-if-string.1
  (count-if #'(lambda (x) (eql x #\0)) "001011101101")
  5)

(deftest count-if-string.2
  (count-if #'identity "001011101101")
  12)

(deftest count-if-string.3
  (count-if #'(lambda (x) (break)) "")
  0)

(deftest count-if-string.4
  (count-if #'identity "001011101101" :key #'(lambda (x) (eql x #\0)))
  5)

(deftest count-if-string.5
  (count-if 'identity "001011101101" :key #'(lambda (x) (eql x #\0)))
  5)

(deftest count-if-string.6
  (count-if #'(lambda (x) (eql x #\0)) "001011101101" :key 'identity)
  5)

(deftest count-if-string.8
  (count-if #'identity "001011101101" :key #'(lambda (x) (eql x #\1)))
  7)

(deftest count-if-string.11
  (let ((c 0))
    (count-if #'evenp "001011101101"
	      :key #'(lambda (x) (+ (if (eql x #\0) 0 1) (incf c)))))
  7)

(deftest count-if-string.12
  (let ((c 0))
    (count-if #'evenp "001011101101"
	      :from-end t
	      :key #'(lambda (x) (+ (if (eql x #\0) 0 1) (incf c)))))
  5)

(deftest count-if-string.13
  (count-if #'(lambda (x) (eql x #\0)) "0111011011100" :start 2)
  4)

(deftest count-if-string.14
  (count-if #'(lambda (x) (eql x #\0)) "0111011011100" :end 7)
  2)
  
(deftest count-if-string.15
  (count-if #'(lambda (x) (eql x #\0)) "0111011011100" :end 7 :start 2)
  1)
  
(deftest count-if-string.16
  (count-if #'(lambda (x) (eql x #\0))
	    "0111011011100" :end 7 :start 2 :from-end t)
  1)

;;; Error tests

(deftest count-if-error.1
  (catch-type-error (count-if #'identity 1))
  type-error)

(deftest count-if-error.2
  (catch-type-error (count-if #'identity 'a))
  type-error)

(deftest count-if-error.3
  (catch-type-error (count-if #'identity #\a))
  type-error)
