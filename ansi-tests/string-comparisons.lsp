;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct  4 06:32:41 2002
;;;; Contains: Tests of string comparison functions

(in-package :cl-test)

(deftest string=.1
  (not (string= "abc" (copy-seq "abc")))
  nil)

(deftest string=.2
  (string= "A" "a")
  nil)

(deftest string=.3
  (not (string= #\a "a"))
  nil)

(deftest string=.4
  (not (string= '|abc| (copy-seq "abc")))
  nil)

(deftest string=.5
  (not (string= (copy-seq "abc") '#:|abc|))
  nil)

;;; Test that it doesn't stop at null characters
(deftest string=.6
  (let ((s1 (copy-seq "abc"))
	(s2 (copy-seq "abd"))
	(c (or (code-char 0) #\a)))
    (setf (char s1 1) c)
    (setf (char s2 1) c)
    (values (length s1) (length s2) (string= s1 s2)))
  3 3 nil)

(deftest string=.7
  (loop for i from 0 to 3
	collect (not (string= "abc" "abd" :start1 0 :end1 i :end2 i)))
  (nil nil nil t))

(deftest string=.8
  (loop for i from 0 to 3
	collect (not (string= "abc" "ab" :end1 i)))
  (t t nil t))

(deftest string=.9
  (loop for i from 0 to 3
	collect (not (string= "abc" "abd" :start2 0 :end2 i :end1 i)))
  (nil nil nil t))

(deftest string=.10
  (loop for i from 0 to 3
	collect (not (string= "ab" "abc" :end2 i)))
  (t t nil t))

(deftest string=.11
  (loop for i from 0 to 3
	collect (not (string= "xyab" "ab" :start1 i)))
  (t t nil t))

(deftest string=.12
  (loop for i from 0 to 3
	collect (not (string= "ab" "xyab" :start2 i)))
  (t t nil t))

(deftest string=.13
  (loop for i from 0 to 3
	collect (not (string= "xyab" "ab" :start1 i :end1 nil)))
  (t t nil t))

(deftest string=.14
  (loop for i from 0 to 3
	collect (not (string= "ab" "xyab" :start2 i :end2 nil)))
  (t t nil t))

;;; Order of evaluation

(deftest string=.order.1
  (let ((i 0) x y)
    (values
     (string= (progn (setf x (incf i)) "abc")
	      (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string=.order.2
  (let ((i 0) a b c d e f)
    (values
     (string= (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :start1 (progn (setf c (incf i)) 0)
	      :start2 (progn (setf d (incf i)) 0)
	      :end1 (progn (setf e (incf i)) nil)
	      :end2 (progn (setf f (incf i)) nil)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string=.order.3
  (let ((i 0) a b c d e f)
    (values
     (string= (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :end2 (progn (setf c (incf i)) nil)
	      :end1 (progn (setf d (incf i)) nil)
	      :start2 (progn (setf e (incf i)) 0)
	      :start1 (progn (setf f (incf i)) 0)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string<=.order.1
  (let ((i 0) x y)
    (values
     (string<= (progn (setf x (incf i)) "abf")
	       (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string<=.order.2
  (let ((i 0) a b c d e f)
    (values
     (string<= (progn (setf a (incf i)) "abf")
	       (progn (setf b (incf i)) "abd")
	       :start1 (progn (setf c (incf i)) 0)
	       :start2 (progn (setf d (incf i)) 0)
	       :end1 (progn (setf e (incf i)) nil)
	       :end2 (progn (setf f (incf i)) nil)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string<=.order.3
  (let ((i 0) a b c d e f)
    (values
     (string<= (progn (setf a (incf i)) "abf")
	       (progn (setf b (incf i)) "abd")
	       :end2 (progn (setf c (incf i)) nil)
	       :end1 (progn (setf d (incf i)) nil)
	       :start2 (progn (setf e (incf i)) 0)
	       :start1 (progn (setf f (incf i)) 0)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string<.order.1
  (let ((i 0) x y)
    (values
     (string< (progn (setf x (incf i)) "abf")
	      (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string<.order.2
  (let ((i 0) a b c d e f)
    (values
     (string< (progn (setf a (incf i)) "abf")
	      (progn (setf b (incf i)) "abd")
	      :start1 (progn (setf c (incf i)) 0)
	      :start2 (progn (setf d (incf i)) 0)
	      :end1 (progn (setf e (incf i)) nil)
	      :end2 (progn (setf f (incf i)) nil)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string<.order.3
  (let ((i 0) a b c d e f)
    (values
     (string< (progn (setf a (incf i)) "abf")
	      (progn (setf b (incf i)) "abd")
	      :end2 (progn (setf c (incf i)) nil)
	      :end1 (progn (setf d (incf i)) nil)
	      :start2 (progn (setf e (incf i)) 0)
	      :start1 (progn (setf f (incf i)) 0)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)


(deftest string/=.order.1
  (let ((i 0) x y)
    (values
     (string/= (progn (setf x (incf i)) "abc")
	       (progn (setf y (incf i)) "abc"))
     i x y))
  nil 2 1 2)

(deftest string/=.order.2
  (let ((i 0) a b c d e f)
    (values
     (string/= (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abc")
	       :start1 (progn (setf c (incf i)) 0)
	       :start2 (progn (setf d (incf i)) 0)
	       :end1 (progn (setf e (incf i)) nil)
	       :end2 (progn (setf f (incf i)) nil)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string/=.order.3
  (let ((i 0) a b c d e f)
    (values
     (string/= (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abc")
	       :end2 (progn (setf c (incf i)) nil)
	       :end1 (progn (setf d (incf i)) nil)
	       :start2 (progn (setf e (incf i)) 0)
	       :start1 (progn (setf f (incf i)) 0)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string>=.order.1
  (let ((i 0) x y)
    (values
     (string<= (progn (setf x (incf i)) "abf")
	       (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string>=.order.2
  (let ((i 0) a b c d e f)
    (values
     (string>= (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abd")
	       :start1 (progn (setf c (incf i)) 0)
	       :start2 (progn (setf d (incf i)) 0)
	       :end1 (progn (setf e (incf i)) nil)
	       :end2 (progn (setf f (incf i)) nil)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string>=.order.3
  (let ((i 0) a b c d e f)
    (values
     (string>= (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abd")
	       :end2 (progn (setf c (incf i)) nil)
	       :end1 (progn (setf d (incf i)) nil)
	       :start2 (progn (setf e (incf i)) 0)
	       :start1 (progn (setf f (incf i)) 0)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string>.order.1
  (let ((i 0) x y)
    (values
     (string> (progn (setf x (incf i)) "abc")
	      (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string>.order.2
  (let ((i 0) a b c d e f)
    (values
     (string> (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :start1 (progn (setf c (incf i)) 0)
	      :start2 (progn (setf d (incf i)) 0)
	      :end1 (progn (setf e (incf i)) nil)
	      :end2 (progn (setf f (incf i)) nil)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string>.order.3
  (let ((i 0) a b c d e f)
    (values
     (string> (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :end2 (progn (setf c (incf i)) nil)
	      :end1 (progn (setf d (incf i)) nil)
	      :start2 (progn (setf e (incf i)) 0)
	      :start1 (progn (setf f (incf i)) 0)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)


(deftest string-equal.order.1
  (let ((i 0) x y)
    (values
     (string-equal (progn (setf x (incf i)) "abc")
	      (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string-equal.order.2
  (let ((i 0) a b c d e f)
    (values
     (string-equal (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :start1 (progn (setf c (incf i)) 0)
	      :start2 (progn (setf d (incf i)) 0)
	      :end1 (progn (setf e (incf i)) nil)
	      :end2 (progn (setf f (incf i)) nil)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-equal.order.3
  (let ((i 0) a b c d e f)
    (values
     (string-equal (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :end2 (progn (setf c (incf i)) nil)
	      :end1 (progn (setf d (incf i)) nil)
	      :start2 (progn (setf e (incf i)) 0)
	      :start1 (progn (setf f (incf i)) 0)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-not-greaterp.order.1
  (let ((i 0) x y)
    (values
     (string-not-greaterp (progn (setf x (incf i)) "abf")
		       (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string-not-greaterp.order.2
  (let ((i 0) a b c d e f)
    (values
     (string-not-greaterp (progn (setf a (incf i)) "abf")
	       (progn (setf b (incf i)) "abd")
	       :start1 (progn (setf c (incf i)) 0)
	       :start2 (progn (setf d (incf i)) 0)
	       :end1 (progn (setf e (incf i)) nil)
	       :end2 (progn (setf f (incf i)) nil)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-not-greaterp.order.3
  (let ((i 0) a b c d e f)
    (values
     (string-not-greaterp (progn (setf a (incf i)) "abf")
	       (progn (setf b (incf i)) "abd")
	       :end2 (progn (setf c (incf i)) nil)
	       :end1 (progn (setf d (incf i)) nil)
	       :start2 (progn (setf e (incf i)) 0)
	       :start1 (progn (setf f (incf i)) 0)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-lessp.order.1
  (let ((i 0) x y)
    (values
     (string-lessp (progn (setf x (incf i)) "abf")
	      (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string-lessp.order.2
  (let ((i 0) a b c d e f)
    (values
     (string-lessp (progn (setf a (incf i)) "abf")
	      (progn (setf b (incf i)) "abd")
	      :start1 (progn (setf c (incf i)) 0)
	      :start2 (progn (setf d (incf i)) 0)
	      :end1 (progn (setf e (incf i)) nil)
	      :end2 (progn (setf f (incf i)) nil)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-lessp.order.3
  (let ((i 0) a b c d e f)
    (values
     (string-lessp (progn (setf a (incf i)) "abf")
	      (progn (setf b (incf i)) "abd")
	      :end2 (progn (setf c (incf i)) nil)
	      :end1 (progn (setf d (incf i)) nil)
	      :start2 (progn (setf e (incf i)) 0)
	      :start1 (progn (setf f (incf i)) 0)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)


(deftest string-not-equal.order.1
  (let ((i 0) x y)
    (values
     (string-not-equal (progn (setf x (incf i)) "abc")
	       (progn (setf y (incf i)) "abc"))
     i x y))
  nil 2 1 2)

(deftest string-not-equal.order.2
  (let ((i 0) a b c d e f)
    (values
     (string-not-equal (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abc")
	       :start1 (progn (setf c (incf i)) 0)
	       :start2 (progn (setf d (incf i)) 0)
	       :end1 (progn (setf e (incf i)) nil)
	       :end2 (progn (setf f (incf i)) nil)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-not-equal.order.3
  (let ((i 0) a b c d e f)
    (values
     (string-not-equal (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abc")
	       :end2 (progn (setf c (incf i)) nil)
	       :end1 (progn (setf d (incf i)) nil)
	       :start2 (progn (setf e (incf i)) 0)
	       :start1 (progn (setf f (incf i)) 0)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-not-lessp.order.1
  (let ((i 0) x y)
    (values
     (string-not-lessp (progn (setf x (incf i)) "abc")
		       (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string-not-lessp.order.2
  (let ((i 0) a b c d e f)
    (values
     (string-not-lessp (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abd")
	       :start1 (progn (setf c (incf i)) 0)
	       :start2 (progn (setf d (incf i)) 0)
	       :end1 (progn (setf e (incf i)) nil)
	       :end2 (progn (setf f (incf i)) nil)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-not-lessp.order.3
  (let ((i 0) a b c d e f)
    (values
     (string-not-lessp (progn (setf a (incf i)) "abc")
	       (progn (setf b (incf i)) "abd")
	       :end2 (progn (setf c (incf i)) nil)
	       :end1 (progn (setf d (incf i)) nil)
	       :start2 (progn (setf e (incf i)) 0)
	       :start1 (progn (setf f (incf i)) 0)
	       )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-greaterp.order.1
  (let ((i 0) x y)
    (values
     (string-greaterp (progn (setf x (incf i)) "abc")
	      (progn (setf y (incf i)) "abd"))
     i x y))
  nil 2 1 2)

(deftest string-greaterp.order.2
  (let ((i 0) a b c d e f)
    (values
     (string-greaterp (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :start1 (progn (setf c (incf i)) 0)
	      :start2 (progn (setf d (incf i)) 0)
	      :end1 (progn (setf e (incf i)) nil)
	      :end2 (progn (setf f (incf i)) nil)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)

(deftest string-greaterp.order.3
  (let ((i 0) a b c d e f)
    (values
     (string-greaterp (progn (setf a (incf i)) "abc")
	      (progn (setf b (incf i)) "abd")
	      :end2 (progn (setf c (incf i)) nil)
	      :end1 (progn (setf d (incf i)) nil)
	      :start2 (progn (setf e (incf i)) 0)
	      :start1 (progn (setf f (incf i)) 0)
	      )
     i a b c d e f))
  nil 6 1 2 3 4 5 6)


;;; Random tests (of all the string comparson functions)

(deftest random-string-comparison-tests
  (loop for cmp in '(= /= < > <= >=)
	append
	(loop for case in '(nil t)
	      collect
	      (list cmp case
		    (random-string-compare-test 10 cmp case 1000))))
  ((= nil 0) (= t 0) (/= nil 0) (/= t 0) (< nil 0) (< t 0)
   (> nil 0) (> t 0) (<= nil 0) (<= t 0) (>= nil 0) (>= t 0)))








  



