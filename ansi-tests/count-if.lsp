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

(deftest count-if-nonsimple-vector.17
  (flet ((%f (x) (eqt x 'a)))
    (let ((s (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
			 :fill-pointer 6)))
      (values (count-if #'%f s)
	      (count-if #'%f s :end nil)
	      (count-if #'%f s :end 4)
	      (count-if #'%f s :start 1)
	      (count-if #'%f s :start 1 :end 4)
	      (count-if #'%f s :start 1 :end 4 :from-end t))))
  2 2 1 1 0 0)

;;; tests on bit-vectors

(deftest count-if-bit-vector.1
  (count-if #'evenp #*001011101101)
  5)

(deftest count-if-bit-vector.2
  (count-if #'identity #*001011101101)
  12)

(deftest count-if-bit-vector.3
  (count-if #'(lambda (x) (break)) #*)
  0)

(deftest count-if-bit-vector.4
  (count-if #'identity #*001011101101 :key #'zerop)
  5)

(deftest count-if-bit-vector.5
  (count-if 'identity #*001011101101 :key #'zerop)
  5)

(deftest count-if-bit-vector.6
  (count-if #'identity #*001011101101 :key 'zerop)
  5)

(deftest count-if-bit-vector.8
  (count-if #'identity #*001011101101 :key 'oddp)
  7)

(deftest count-if-bit-vector.10
  (count-if #'evenp #*001011101101 :key #'1+)
  7)

(deftest count-if-bit-vector.11
  (let ((c 0))
    (count-if #'evenp #*001011101101
	      :key #'(lambda (x) (+ x (incf c)))))
  7)

(deftest count-if-bit-vector.12
  (let ((c 0))
    (count-if #'evenp #*001011101101
	      :from-end t
	      :key #'(lambda (x) (+ x (incf c)))))
  5)

(deftest count-if-bit-vector.13
  (count-if #'zerop #*0111011011100 :start 2)
  4)

(deftest count-if-bit-vector.14
  (count-if #'zerop #*0111011011100 :end 7)
  2)
  
(deftest count-if-bit-vector.15
  (count-if #'zerop #*0111011011100 :end 7 :start 2)
  1)
  
(deftest count-if-bit-vector.16
  (count-if #'zerop #*0111011011100 :end 7 :start 2 :from-end t)
  1)

(deftest count-if-bit-vector.17
  (let ((s (make-array '(10) :initial-contents '(0 0 1 0 1 0 0 1 1 0)
		       :element-type 'bit
		       :fill-pointer 6)))
    (values (count-if #'zerop s)
	    (count-if #'zerop s :end nil)
	    (count-if #'zerop s :end 4)
	    (count-if #'zerop s :start 5)
	    (count-if #'zerop s :start 1 :end 4)))
  4 4 3 1 2)

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

(deftest count-if-string.17
  (let ((s (make-array '(10)
		       :initial-contents "00a0aa0a0a"
		       :element-type 'character
		       :fill-pointer 6)))
    (values (count-if #'digit-char-p s)
	    (count-if #'digit-char-p s :end nil)
	    (count-if #'digit-char-p s :start 1)
	    (count-if #'digit-char-p s :end 2)
	    (count-if #'digit-char-p s :start 1 :end 2)))
  3 3 2 2 1)

;;; Argument order tests

(deftest count-if.order.1
  (let ((i 0) c1 c2 c3 c4 c5 c6)
    (values
     (count-if (progn (setf c1 (incf i)) #'null)
	       (progn (setf c2 (incf i)) '(a nil b c nil d e))
	       :start (progn (setf c3 (incf i)) 0)
	       :end (progn (setf c4 (incf i)) 3)
	       :key (progn (setf c5 (incf i)) #'identity)
	       :from-end (progn (setf c6 (incf i)) nil)
	       )
     i c1 c2 c3 c4 c5 c6))
  1 6 1 2 3 4 5 6)

(deftest count-if.order.2
  (let ((i 0) c1 c2 c3 c4 c5 c6)
    (values
     (count-if (progn (setf c1 (incf i)) #'null)
	       (progn (setf c2 (incf i)) '(a nil b c nil d e))
	       :from-end (progn (setf c3 (incf i)) nil)
	       :key (progn (setf c4 (incf i)) #'identity)
	       :end (progn (setf c5 (incf i)) 3)
	       :start (progn (setf c6 (incf i)) 0)
	       )
     i c1 c2 c3 c4 c5 c6))
  1 6 1 2 3 4 5 6)


;;; Keyword tests

(deftest count-if.allow-other-keys.1
  (count-if #'evenp '(1 2 3 4 5) :bad t :allow-other-keys t)
  2)

(deftest count-if.allow-other-keys.2
  (count-if #'evenp '(1 2 3 4 5) :allow-other-keys #p"*" :also-bad t)
  2)

;;; The leftmost of two :allow-other-keys arguments is the one that  matters.
(deftest count-if.allow-other-keys.3
  (count-if #'evenp '(1 2 3 4 5)
	    :allow-other-keys t
	    :allow-other-keys nil
	    :bad t)
  2)

(deftest count-if.keywords.4
  (count-if #'evenp '(1 2 3 4 5) :key #'identity :key #'1+)
  2)

(deftest count-if.allow-other-keys.5
  (count-if #'evenp '(1 2 3 4 5) :allow-other-keys nil)
  2)

	    
;;; Error tests

(deftest count-if.error.1
  (classify-error (count-if #'identity 1))
  type-error)

(deftest count-if.error.2
  (classify-error (count-if #'identity 'a))
  type-error)

(deftest count-if.error.3
  (classify-error (count-if #'identity #\a))
  type-error)

(deftest count-if.error.4
  (classify-error (count-if))
  program-error)

(deftest count-if.error.5
  (classify-error (count-if #'null))
  program-error)

(deftest count-if.error.6
  (classify-error (count-if #'null nil :bad t))
  program-error)

(deftest count-if.error.7
  (classify-error (count-if #'null nil :bad t :allow-other-keys nil))
  program-error)

(deftest count-if.error.8
  (classify-error (count-if #'null nil :key))
  program-error)

(deftest count-if.error.9
  (classify-error (count-if #'null nil 3 3))
  program-error)

;;; Only leftmost :allow-other-keys argument matters
(deftest count-if.error.10
  (classify-error (count-if #'null nil :bad t
			    :allow-other-keys nil
			    :allow-other-keys t))
  program-error)

(deftest count-if.error.11
  (classify-error (locally (count-if #'identity 1) t))
  type-error)

(deftest count-if.error.12
  (classify-error (count-if #'cons '(a b c)))
  program-error)

(deftest count-if.error.13
  (classify-error (count-if #'car '(a b c)))
  type-error)

(deftest count-if.error.14
  (classify-error (count-if #'identity '(a b c) :key #'cdr))
  type-error)

(deftest count-if.error.15
  (classify-error (count-if #'identity '(a b c) :key #'cons))
  program-error)


