;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug 27 16:11:38 2002
;;;; Contains: Tests for REPLACE

(in-package :cl-test)

(deftest replace-list.1
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x '(x y z))))
    (values (eq x result) result))
  t
  (x y z d e f g))

(deftest replace-list.2
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x '(x y z) :start1 1)))
    (values (eq x result) result))
  t
  (a x y z e f g))

(deftest replace-list.3
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x '(x y z) :start1 4)))
    (values (eq x result) result))
  t
  (a b c d x y z))

(deftest replace-list.4
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x '(x y z) :start1 5)))
    (values (eq x result) result))
  t
  (a b c d e x y))

(deftest replace-list.5
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x '(x y z) :start1 6)))
    (values (eq x result) result))
  t
  (a b c d e f x))

(deftest replace-list.6
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x #(x y z) :start1 2)))
    (values (eq x result) result))
  t
  (a b x y z f g))

(deftest replace-list.7
  (replace nil #(x y z))
  nil)

(deftest replace-list.8
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x '(x y z) :end1 1)))
    (values (eq x result) result))
  t
  (x b c d e f g))

(deftest replace-list.9
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x '(x y z) :start1 3 :end1 4)))
    (values (eq x result) result))
  t
  (a b c x e f g))

(deftest replace-list.10
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x '(x y z) :start1 0 :end1 5)))
    (values (eq x result) result))
  t
  (x y z d e f g))


(deftest replace-list.11
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x '(x y z) :start2 1)))
    (values (eq x result) result))
  t
  (y z c d e f g))

(deftest replace-list.12
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x '(x y z) :start2 1 :end1 nil)))
    (values (eq x result) result))
  t
  (y z c d e f g))

(deftest replace-list.13
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x '(x y z) :start2 1 :end2 nil)))
    (values (eq x result) result))
  t
  (y z c d e f g))

(deftest replace-list.14
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x '(x y z) :start2 1 :end2 2)))
    (values (eq x result) result))
  t
  (y b c d e f g))

(deftest replace-list.15
  (let* ((x (copy-seq '(a b c d e f g)))
	 (result (replace x '(x y z) :start1 4 :end1 5 :start2 1 :end2 2)))
    (values (eq x result) result))
  t
  (a b c d y f g))


;;; Tests of vectors

(deftest replace-vector.1
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x #(x y z))))
    (values (eq x result) result))
  t
  #(x y z d e f g))

(deftest replace-vector.2
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x #(x y z) :start1 1)))
    (values (eq x result) result))
  t
  #(a x y z e f g))

(deftest replace-vector.3
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x #(x y z) :start1 4)))
    (values (eq x result) result))
  t
  #(a b c d x y z))

(deftest replace-vector.4
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x #(x y z) :start1 5)))
    (values (eq x result) result))
  t
  #(a b c d e x y))

(deftest replace-vector.5
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x #(x y z) :start1 6)))
    (values (eq x result) result))
  t
  #(a b c d e f x))

(deftest replace-vector.6
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x '(x y z) :start1 2)))
    (values (eq x result) result))
  t
  #(a b x y z f g))

(deftest replace-vector.7
  (replace #() #(x y z))
  #())

(deftest replace-vector.8
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x #(x y z) :end1 1)))
    (values (eq x result) result))
  t
  #(x b c d e f g))

(deftest replace-vector.9
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x #(x y z) :start1 3 :end1 4)))
    (values (eq x result) result))
  t
  #(a b c x e f g))

(deftest replace-vector.10
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x #(x y z) :start1 0 :end1 5)))
    (values (eq x result) result))
  t
  #(x y z d e f g))


(deftest replace-vector.11
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x #(x y z) :start2 1)))
    (values (eq x result) result))
  t
  #(y z c d e f g))

(deftest replace-vector.12
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x #(x y z) :start2 1 :end1 nil))) 
    (values (eq x result) result))
  t
  #(y z c d e f g))

(deftest replace-vector.13
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x #(x y z) :start2 1 :end2 nil)))
    (values (eq x result) result))
  t
  #(y z c d e f g))

(deftest replace-vector.14
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x #(x y z) :start2 1 :end2 2)))
    (values (eq x result) result))
  t
  #(y b c d e f g))

(deftest replace-vector.15
  (let* ((x (copy-seq #(a b c d e f g)))
	 (result (replace x #(x y z) :start1 4 :end1 5 :start2 1 :end2 2)))
    (values (eq x result) result))
  t
  #(a b c d y f g))

;;; tests on bit strings

(deftest replace-bitstring.1
  (let* ((x (copy-seq #*1101001))
	 (result (replace x #*011)))
    (values (eq x result) result))
  t
  #*0111001)

(deftest replace-bitstring.2
  (let* ((x (copy-seq #*1101001))
	 (result (replace x #*011 :start1 1)))
    (values (eq x result) result))
  t
  #*1011001)

(deftest replace-bitstring.3
  (let* ((x (copy-seq #*1101001))
	 (result (replace x #*011 :start1 4)))
    (values (eq x result) result))
  t
  #*1101011)

(deftest replace-bitstring.4
  (let* ((x (copy-seq #*0000000))
	 (result (replace x #*111 :start1 5)))
    (values (eq x result) result))
  t
  #*0000011)

(deftest replace-bitstring.5
  (let* ((x (copy-seq #*0000000))
	 (result (replace x #*100 :start1 6)))
    (values (eq x result) result))
  t
  #*0000001)

(deftest replace-bitstring.6
  (let* ((x (copy-seq #*0000000))
	 (result (replace x '(1 1 1) :start1 2)))
    (values (eq x result) result))
  t
  #*0011100)

(deftest replace-bitstring.7
  (replace #* #*111)
  #*)

(deftest replace-bitstring.8
  (let* ((x (copy-seq #*0000000))
	 (result (replace x #*111 :end1 1)))
    (values (eq x result) result))
  t
  #*1000000)

(deftest replace-bitstring.9
  (let* ((x (copy-seq #*0000000))
	 (result (replace x #*110 :start1 3 :end1 4)))
    (values (eq x result) result))
  t
  #*0001000)

(deftest replace-bitstring.10
  (let* ((x (copy-seq #*0000000))
	 (result (replace x #*111 :start1 0 :end1 5)))
    (values (eq x result) result))
  t
  #*1110000)


(deftest replace-bitstring.11
  (let* ((x (copy-seq #*0000000))
	 (result (replace x #*011 :start2 1)))
    (values (eq x result) result))
  t
  #*1100000)

(deftest replace-bitstring.12
  (let* ((x (copy-seq #*0000000))
	 (result (replace x #*011 :start2 1 :end1 nil)))
    (values (eq x result) result))
  t
  #*1100000)

(deftest replace-bitstring.13
  (let* ((x (copy-seq #*0000000))
	 (result (replace x #*011 :start2 1 :end2 nil)))
    (values (eq x result) result))
  t
  #*1100000)

(deftest replace-bitstring.14
  (let* ((x (copy-seq #*0000000))
	 (result (replace x #*011 :start2 1 :end2 2)))
    (values (eq x result) result))
  t
  #*1000000)

(deftest replace-bitstring.15
  (let* ((x (copy-seq #*0000000))
	 (result (replace x #*011 :start1 4 :end1 5 :start2 1 :end2 2)))
    (values (eq x result) result))
  t
  #*0000100)

;;; Tests on strings

(deftest replace-string.1
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x "xyz")))
    (values (eq x result) result))
  t
  "xyzdefg")

(deftest replace-string.2
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x "xyz" :start1 1)))
    (values (eq x result) result))
  t
  "axyzefg")

(deftest replace-string.3
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x "xyz" :start1 4)))
    (values (eq x result) result))
  t
  "abcdxyz")

(deftest replace-string.4
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x "xyz" :start1 5)))
    (values (eq x result) result))
  t
  "abcdexy")

(deftest replace-string.5
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x "xyz" :start1 6)))
    (values (eq x result) result))
  t
  "abcdefx")

(deftest replace-string.6
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x '(#\x #\y #\z) :start1 2)))
    (values (eq x result) result))
  t
  "abxyzfg")

(deftest replace-string.7
  (replace "" "xyz")
  "")

(deftest replace-string.8
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x "xyz" :end1 1)))
    (values (eq x result) result))
  t
  "xbcdefg")

(deftest replace-string.9
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x "xyz" :start1 3 :end1 4)))
    (values (eq x result) result))
  t
  "abcxefg")

(deftest replace-string.10
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x "xyz" :start1 0 :end1 5)))
    (values (eq x result) result))
  t
  "xyzdefg")


(deftest replace-string.11
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x "xyz" :start2 1)))
    (values (eq x result) result))
  t
  "yzcdefg")

(deftest replace-string.12
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x "xyz" :start2 1 :end1 nil))) 
    (values (eq x result) result))
  t
  "yzcdefg")

(deftest replace-string.13
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x "xyz" :start2 1 :end2 nil)))
    (values (eq x result) result))
  t
  "yzcdefg")

(deftest replace-string.14
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x "xyz" :start2 1 :end2 2)))
    (values (eq x result) result))
  t
  "ybcdefg")

(deftest replace-string.15
  (let* ((x (copy-seq "abcdefg"))
	 (result (replace x "xyz" :start1 4 :end1 5 :start2 1 :end2 2)))
    (values (eq x result) result))
  t
  "abcdyfg")
