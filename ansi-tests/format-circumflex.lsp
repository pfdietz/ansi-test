;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Nov 11 20:17:51 2004
;;;; Contains: Tests of the ~^ format directive (inside other format constructs)

(in-package :cl-test)
(compile-and-load "printer-aux.lsp")

;;; Tests of ~^ inside ~{ ... ~}

(deftest format.^.{.1
  (format nil "~{X ~A~^ Y ~A~^ ~}" '(1 2 3 4 5))
  "X 1 Y 2 X 3 Y 4 X 5")

(deftest format.^.{.2
  (format nil "~{X ~A~^ Y ~A~^ ~}" '(1 2 3 4))
  "X 1 Y 2 X 3 Y 4")

(deftest format.^.{.3
  (format nil "~1{~A~^~A~}" '(1))
  "1")

(deftest format.^.{.4
  (format nil "~0{~A~^~A~}" '(1))
  "")

(deftest format.^.{.5
  (format nil "~1{~A~^~A~}" '(1 2 3))
  "12")

(deftest format.^.{.6
  (format nil "~{~A~A~0^~A~}" '(1 2 3 4 5 6))
  "12")

(deftest format.^.{.7
  (format nil "~{~A~A~v^~A~}" '(1 2 3 4 5 6 0 7 8 9 10 11 12))
  "12456")

(deftest format.^.{.8
  (format nil "~{~#,3^~A~}" '(1 2 3 4 5 6 7 8 9 10))
  "1234567")

(deftest format.^.{.9
  (format nil "~{~2,#^~A~}~A" '(1 2 3 4 5 6 7 8 9 10) 0)
  "123456780")

(deftest format.^.{.10
  (format nil "~{~#,#^~A~}" '(1 2 3 4 5 6 7 8 9 10))
  "")

(deftest format.^.{.11
  (format nil "~{~#,#,#^~A~}" '(1 2 3 4 5 6 7 8 9 10))
  "")

(deftest format.^.{.12
  (format nil "~{~#,1,2^~A~}" '(1 2 3 4 5 6 7 8 9 10))
  "123456789")

(deftest format.^.{.13
  (format nil "~{~#,#,v^~A~}" '(1 2 3 4 5 6 7 8 9 10))
  "246")

(deftest format.^.{.14
  (format nil "~{~#,#,v^~A~}" '(1 2 3 4 5 6 7 8 9 10 11))
  "246")

(deftest format.^.{.15
  (format nil "~{~#,#,v^~A~}" '(1 2 3 4 5 6 7 8 9 10 11 12))
  "246")

(deftest format.^.{.16
  (format nil "~{~#,#,v^~A~}" '(1 2 3 4 5 6 7 8 9 10 11 12 13))
  "246")

(deftest format.^.{.17
  (format nil "~{~#,#,v^~A~}" '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
  "2468")

(deftest format.^.{.18
  (format nil "~{~v,v^~A~}" (list (1+ most-positive-fixnum)
				  (1+ most-positive-fixnum)
				  1))
  "")

(deftest format.^.{.19
  (format nil "~{~0,v,v^~A~}" (list (1+ most-positive-fixnum)
				  (1+ most-positive-fixnum)
				  1))
  "")

(deftest format.^.{.20
  (format nil "~{~0,v,v^~A~}" (list (1+ most-positive-fixnum)
				    most-positive-fixnum
				    1))
  "1")

(deftest format.^.{.21
  (format nil "~{~1,v^~A~}" '(nil 8 nil 7 0 6 1 5))
  "876")

(deftest format.^.{.22
  (format nil "~{~0,v^~A~}" '(3 8 1 7 3 6 nil 5))
  "876")

(deftest format.^.{.23
  (format nil "~{~1,2,v^~A~}" '(0 1 0 2 0 3 3 4))
  "123")

(deftest format.^.{.24
  (format nil "~{~1,2,v^~A~}" '(0 1 0 2 0 3 nil 4))
  "1234")

(deftest format.^.{.25
  (format nil "~{~1,1,v^~A~}" '(0 1 0 2 0 3 nil 4))
  "123")

(deftest format.^.{.26
  (format nil "~{~'X^~A~}" '(1 2 3))
  "123")

(deftest format.^.{.27
  (format nil "~{~v,'X^~A~}" '(0 1 #\x 2 nil 3 #\X 4 0 5))
  "123")

(deftest format.^.{.28
  (format nil "~{~'X,v^~A~}" '(0 1 #\x 2 nil 3 #\X 4 0 5))
  "123")

(deftest format.^.{.29
  (format nil "~{~v,v^~A~}" '(0 2 1 #\x #\X 2 5 #\X 3 #\y #\y 4 1 2 5))
  "123")

(deftest format.^.{.30
  (format nil "~{~',,',^~A~}" '(1 2 3))
  "")

(deftest format.^.{.31
  (format nil "~{~1,v,v^~A~}" '(#\a nil 0))
  "0")

(deftest format.^.{.32
  (format nil "~{~v,1,v^~A~}" '(#\a nil 0))
  "0")

(deftest format.^.{.33
  (format nil "~{~v,v,v^~A~}" '(#\a #\a nil 0))
  "")

;;; ~^ with ~:{

(deftest format.^.\:{.1
  (format nil "~:{~A~^~A~A~}" '((1)(2 3 4)(5 6 7 8)))
  "1234567")

(deftest format.^.\:{.2
  (format nil "~:{~A~0^~A~A~}" '((1)(2 3 4)(5 6 7 8)))
  "125")

(deftest format.^.\:{.3
  (format nil "~:{~#^~A~}" '((1)(2 3 4)()(5 6 7 8))())
  "125")

(deftest format.^.\:{.4
  (format nil "~:{~#^~A~#^~A~#^~A~#^~A~}" '((1)(2 3 4)()(5 6 7 8))())
  "12345678")

(deftest format.^.\:{.5
  (format nil "~:{~v^~A~}" '((1 2 3)(0)(2 4)(0 5)(1 6 7 8)))
  "246")

(deftest format.^.\:{.6
  (format nil "~:{~v^~A~}" '((nil)(nil 1)(1 2)))
  "12")

(deftest format.^.\:{.7
  (format nil "~:{~v^~A~}" '((#\x 1)(#\y 2)(0 3)(1 4)))
  "124")

(deftest format.^.\:{.8
  (format nil "~:{~v,3^~A~}" '((1 1)(2 0)(3 4)(5 6)))
  "106")

(deftest format.^.\:{.9
  (format nil "~:{~3,v^~A~}" '((1 1)(2 0)(3 4)(5 6)))
  "106")

(deftest format.^.\:{.10
  (format nil "~:{~v,3^~A~}" '((#\x 1)))
  "1")

(deftest format.^.\:{.11
  (format nil "~:{~2,v^~A~}" '((#\x 1)))
  "1")

(deftest format.^.\:{.12
  (format nil "~:{~v,v^~A~}" '((1 2 0) (0 1 1) (1 0 2) (3 3 5) (4 5 6)))
  "0126")

(deftest format.^.\:{.13
  (format nil "~:{~v,v^~A~}" '((1 2 0) (#\a #\A 1) (#\A #\A 2) (1 2 3)))
  "013")

(deftest format.^.\:{.14
  (format nil "~:{~'x,3^~A~}" '((1)))
  "1")

(deftest format.^.\:{.15
  (format nil "~:{~3,'x^~A~}" '((1)))
  "1")

(deftest format.^.\:{.16
  (format nil "~:{~'x,'x^~A~}" '((1)))
  "")

(deftest format.^.\:{.17
  (format nil "~:{~#,1^~A~}" '((1)(2 10)(3 a b)(4)(5 x)(6)(7 8)))
  "2357")

(deftest format.^.\:{.18
  (format nil "~:{~1,#^~A~}" '((1)(2 10)(3 a b)(4)(5 x)(6)(7 8)))
  "2357")

(deftest format.^.\:{.19
  (format nil "~:{~#,#^~A~}" '((1)()(2 10)(3 a b)(4)(5 x)(6)(7 8)))
  "")

(deftest format.^.\:{.20
  (format nil "~:{~0,v^~A~}" '((0 1)(1 2)(nil 3)(2 4)))
  "24")

(deftest format.^.\:{.21
  (format nil "~:{~1,v^~A~}" '((0 1)(1 2)(nil 3)(2 4)))
  "134")

(deftest format.^.\:{.22
  (format nil "~:{~1,1,1^~A~}" '((1)(2 3)(4 5 6)(7 8 9 0)))
  "")

(deftest format.^.\:{.23
  (format nil "~:{~1,2,3^~A~}" '((1)(2 3)(4 5 6)(7 8 9 0)))
  "")

(deftest format.^.\:{.24
  (format nil "~:{~1,2,1^~A~}" '((1)(2 3)(4 5 6)(7 8 9 0)))
  "1247")

(deftest format.^.\:{.25
  (format nil "~:{~1,0,1^~A~}" '((1)(2 3)(4 5 6)(7 8 9 0)))
  "1247")

(deftest format.^.\:{.26
  (format nil "~:{~3,2,1^~A~}" '((1)(2 3)(4 5 6)(7 8 9 0)))
  "1247")

(deftest format.^.\:{.27
  (format nil "~:{~v,2,3^~A~}" '((1 10)(2 20)(3 30)(4 40)))
  "3040")

(deftest format.^.\:{.28
  (format nil "~:{~1,v,3^~A~}" '((0 7)(1 10)(2 20)(3 30)(4 40)))
  "740")

(deftest format.^.\:{.29
  (format nil "~:{~1,2,v^~A~}" '((0 0)(1 10)(2 20)(3 30)(4 40)(0 50)))
  "01050")

(deftest format.^.\:{.30
  (format nil "~:{~1,2,v^~A~}" '((nil 0)))
  "0")

(deftest format.^.\:{.31
  (format nil "~:{~#,3,3^~A~}" '((1) (2 1) (3 2 1) (4 3 2 1) (5 4 3 2 1)))
  "45")

(deftest format.^.\:{.32
  (format nil "~:{~2,#,3^~A~}" '((1) (2 1) (3 2 1) (4 3 2 1) (5 4 3 2 1)))
  "145")

(deftest format.^.\:{.33
  (format nil "~:{~0,3,#^~A~}" '((1) (2 1) (3 2 1) (4 3 2 1) (5 4 3 2 1)))
  "12")

(deftest format.^.\:{.34
  (format nil "~:{~#,#,3^~A~}" '((1) (2 1) (3 2 1) (4 3 2 1) (5 4 3 2 1)))
  "45")

(deftest format.^.\:{.35
  (format nil "~:{~3,#,#^~A~}" '((1) (2 1) (3 2 1) (4 3 2 1) (5 4 3 2 1)))
  "12")

(deftest format.^.\:{.36
  (format nil "~:{~#,3,#^~A~}" '((1) (2 1) (3 2 1) (4 3 2 1) (5 4 3 2 1)))
  "1245")

(deftest format.^.\:{.37
  (format nil "~:{~#,#,#^~A~}" '((1) (2 1) (3 2 1) (4 3 2 1) (5 4 3 2 1)))
  "")

(deftest format.^.\:{.38
  (format nil "~:{~1,v,v^~A~}" '((#\a nil 0)))
  "0")

(deftest format.^.\:{.39
  (format nil "~:{~v,1,v^~A~}" '((#\a nil 0)))
  "0")

;;; Tests of ~^ inside ~@{ ... ~}

(deftest format.^.@{.1
  (format nil "~@{X ~A~^ Y ~A~^ ~}" 1 2 3 4 5)
  "X 1 Y 2 X 3 Y 4 X 5")

(deftest format.^.@{.2
  (format nil "~@{X ~A~^ Y ~A~^ ~}" 1 2 3 4)
  "X 1 Y 2 X 3 Y 4")

(deftest format.^.@{.3
  (format nil "~1@{~A~^~A~}" 1)
  "1")

(deftest format.^.@{.4
  (format nil "~0@{~A~^~A~}" 1)
  "")

(deftest format.^.@{.5
  (format nil "~1@{~A~^~A~}" 1 2 3)
  "12")

(deftest format.^.@{.6
  (format nil "~@{~A~A~0^~A~}" 1 2 3 4 5 6)
  "12")

(deftest format.^.@{.7
  (format nil "~@{~A~A~v^~A~}" 1 2 3 4 5 6 0 7 8 9 10 11 12)
  "12456")

(deftest format.^.@{.8
  (format nil "~@{~#,3^~A~}" 1 2 3 4 5 6 7 8 9 10)
  "1234567")

(deftest format.^.@{.9
  (format nil "~@{~2,#^~A~}X~A" 1 2 3 4 5 6 7 8 9 10)
  "12345678X9")

(deftest format.^.@{.10
  (format nil "~@{~#,#^~A~}" 1 2 3 4 5 6 7 8 9 10)
  "")

(deftest format.^.@{.11
  (format nil "~@{~#,#,#^~A~}" 1 2 3 4 5 6 7 8 9 10)
  "")

(deftest format.^.@{.12
  (format nil "~@{~#,1,2^~A~}" 1 2 3 4 5 6 7 8 9 10)
  "123456789")

(deftest format.^.@{.13
  (format nil "~@{~#,#,v^~A~}" 1 2 3 4 5 6 7 8 9 10)
  "246")

(deftest format.^.@{.14
  (format nil "~@{~#,#,v^~A~}" 1 2 3 4 5 6 7 8 9 10 11)
  "246")

(deftest format.^.@{.15
  (format nil "~@{~#,#,v^~A~}" 1 2 3 4 5 6 7 8 9 10 11 12)
  "246")

(deftest format.^.@{.16
  (format nil "~@{~#,#,v^~A~}" 1 2 3 4 5 6 7 8 9 10 11 12 13)
  "246")

(deftest format.^.@{.17
  (format nil "~@{~#,#,v^~A~}" 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
  "2468")

(deftest format.^.@{.18
  (format nil "~@{~v,v^~A~}"
	  (1+ most-positive-fixnum)
	  (1+ most-positive-fixnum)
	  1)
  "")

(deftest format.^.@{.19
  (format nil "~@{~0,v,v^~A~}"
	  (1+ most-positive-fixnum)
	  (1+ most-positive-fixnum)
	  1)
  "")

(deftest format.^.@{.20
  (format nil "~@{~0,v,v^~A~}"
	  (1+ most-positive-fixnum)
	  most-positive-fixnum
	  1)
  "1")

(deftest format.^.@{.21
  (format nil "~@{~1,v^~A~}" nil 8 nil 7 0 6 1 5)
  "876")

(deftest format.^.@{.22
  (format nil "~@{~0,v^~A~}" 3 8 1 7 3 6 nil 5)
  "876")

(deftest format.^.@{.23
  (format nil "~@{~1,2,v^~A~}" 0 1 0 2 0 3 3 4)
  "123")

(deftest format.^.@{.24
  (format nil "~@{~1,2,v^~A~}" 0 1 0 2 0 3 nil 4)
  "1234")

(deftest format.^.@{.25
  (format nil "~@{~1,1,v^~A~}" 0 1 0 2 0 3 nil 4)
  "123")

(deftest format.^.@{.26
  (format nil "~@{~'X^~A~}" 1 2 3)
  "123")

(deftest format.^.@{.27
  (format nil "~@{~v,'X^~A~}" 0 1 #\x 2 nil 3 #\X 4 0 5)
  "123")

(deftest format.^.@{.28
  (format nil "~@{~'X,v^~A~}" 0 1 #\x 2 nil 3 #\X 4 0 5)
  "123")

(deftest format.^.@{.29
  (format nil "~@{~v,v^~A~}" 0 2 1 #\x #\X 2 5 #\X 3 #\y #\y 4 1 2 5)
  "123")

(deftest format.^.@{.30
  (format nil "~@{~',,',^~A~}" 1 2 3)
  "")

(deftest format.^.@{.31
  (format nil "~@{~1,v,v^~A~}" #\a nil 0)
  "0")

(deftest format.^.@{.32
  (format nil "~@{~v,1,v^~A~}" #\a nil 0)
  "0")

(deftest format.^.@{.33
  (format nil "~@{~v,v,v^~A~}" #\a #\a nil 0)
  "")

;;; Inside ~:@{

(deftest format.^.\:@{.1
  (format nil "~:@{~A~^~A~A~}" '(1) '(2 3 4) '(5 6 7 8))
  "1234567")

(deftest format.^.\:@{.2
  (format nil "~@:{~A~0^~A~A~}" '(1) '(2 3 4) '(5 6 7 8))
  "125")

(deftest format.^.\:@{.3
  (format nil "~:@{~#^~A~}" '(1) '(2 3 4) () '(5 6 7 8) ())
  "125")

(deftest format.^.\:@{.4
  (format nil "~@:{~#^~A~#^~A~#^~A~#^~A~}" '(1) '(2 3 4) () '(5 6 7 8) ())
  "12345678")

(deftest format.^.\:@{.5
  (format nil "~:@{~v^~A~}" '(1 2 3) '(0) '(2 4) '(0 5) '(1 6 7 8))
  "246")

(deftest format.^.\:@{.6
  (format nil "~:@{~v^~A~}" '(nil) '(nil 1) '(1 2))
  "12")

(deftest format.^.\:@{.7
  (format nil "~:@{~v^~A~}" '(#\x 1) '(#\y 2) '(0 3) '(1 4))
  "124")

(deftest format.^.\:@{.8
  (format nil "~:@{~v,3^~A~}" '(1 1) '(2 0) '(3 4) '(5 6))
  "106")

(deftest format.^.\:@{.9
  (format nil "~@:{~3,v^~A~}" '(1 1) '(2 0) '(3 4) '(5 6))
  "106")

(deftest format.^.\:@{.10
  (format nil "~:@{~v,3^~A~}" '(#\x 1))
  "1")

(deftest format.^.\:@{.11
  (format nil "~:@{~2,v^~A~}" '(#\x 1))
  "1")

(deftest format.^.\:@{.12
  (format nil "~:@{~v,v^~A~}" '(1 2 0) '(0 1 1) '(1 0 2) '(3 3 5) '(4 5 6))
  "0126")

(deftest format.^.\:@{.13
  (format nil "~:@{~v,v^~A~}" '(1 2 0) '(#\a #\A 1) '(#\A #\A 2) '(1 2 3))
  "013")

(deftest format.^.\:@{.14
  (format nil "~:@{~'x,3^~A~}" '(1))
  "1")

(deftest format.^.\:@{.15
  (format nil "~:@{~3,'x^~A~}" '(1))
  "1")

(deftest format.^.\:@{.16
  (format nil "~:@{~'x,'x^~A~}" '(1))
  "")

(deftest format.^.\:@{.17
  (format nil "~:@{~#,1^~A~}" '(1) '(2 10) '(3 a b) '(4) '(5 x) '(6) '(7 8))
  "2357")

(deftest format.^.\:@{.18
  (format nil "~:@{~1,#^~A~}" '(1) '(2 10) '(3 a b) '(4) '(5 x) '(6) '(7 8))
  "2357")

(deftest format.^.\:@{.19
  (format nil "~:@{~#,#^~A~}" '(1) '() '(2 10) '(3 a b) '(4) '(5 x) '(6) '(7 8))
  "")

(deftest format.^.\:@{.20
  (format nil "~:@{~0,v^~A~}" '(0 1) '(1 2) '(nil 3) '(2 4))
  "24")

 (deftest format.^.\:@{.21
  (format nil "~:@{~1,v^~A~}" '(0 1) '(1 2) '(nil 3) '(2 4))
  "134")

 (deftest format.^.\:@{.22
  (format nil "~:@{~1,1,1^~A~}" '(1) '(2 3) '(4 5 6) '(7 8 9 0))
  "")

(deftest format.^.\:@{.23
  (format nil "~:@{~1,2,3^~A~}" '(1) '(2 3) '(4 5 6) '(7 8 9 0))
  "")

(deftest format.^.\:@{.24
  (format nil "~:@{~1,2,1^~A~}" '(1) '(2 3) '(4 5 6) '(7 8 9 0))
  "1247")

(deftest format.^.\:@{.25
  (format nil "~:@{~1,0,1^~A~}" '(1) '(2 3) '(4 5 6) '(7 8 9 0))
  "1247")

(deftest format.^.\:@{.26
  (format nil "~:@{~3,2,1^~A~}" '(1) '(2 3) '(4 5 6) '(7 8 9 0))
  "1247")

(deftest format.^.\:@{.27
  (format nil "~:@{~v,2,3^~A~}" '(1 10) '(2 20) '(3 30) '(4 40))
  "3040")

(deftest format.^.\:@{.28
  (format nil "~:@{~1,v,3^~A~}" '(0 7) '(1 10) '(2 20) '(3 30) '(4 40))
  "740")

(deftest format.^.\:@{.29
  (format nil "~:@{~1,2,v^~A~}" '(0 0) '(1 10) '(2 20) '(3 30) '(4 40) '(0 50))
  "01050")

(deftest format.^.\:@{.30
  (format nil "~:@{~1,2,v^~A~}" '(nil 0))
  "0")

(deftest format.^.\:@{.31
  (format nil "~:@{~#,3,3^~A~}" '(1) '(2 1) '(3 2 1) '(4 3 2 1) '(5 4 3 2 1))
  "45")

(deftest format.^.\:@{.32
  (format nil "~:@{~2,#,3^~A~}" '(1) '(2 1) '(3 2 1) '(4 3 2 1) '(5 4 3 2 1))
  "145")

(deftest format.^.\:@{.33
  (format nil "~:@{~0,3,#^~A~}" '(1) '(2 1) '(3 2 1) '(4 3 2 1) '(5 4 3 2 1))
  "12")

(deftest format.^.\:@{.34
  (format nil "~:@{~#,#,3^~A~}" '(1) '(2 1) '(3 2 1) '(4 3 2 1) '(5 4 3 2 1))
  "45")

(deftest format.^.\:@{.35
  (format nil "~:@{~3,#,#^~A~}" '(1) '(2 1) '(3 2 1) '(4 3 2 1) '(5 4 3 2 1))
  "12")

(deftest format.^.\:@{.36
  (format nil "~:@{~#,3,#^~A~}" '(1) '(2 1) '(3 2 1) '(4 3 2 1) '(5 4 3 2 1))
  "1245")

(deftest format.^.\:@{.37
  (format nil "~:@{~#,#,#^~A~}" '(1) '(2 1) '(3 2 1) '(4 3 2 1) '(5 4 3 2 1))
  "")

(deftest format.^.\:@{.38
  (format nil "~:@{~1,v,v^~A~}" '(#\a nil 0))
  "0")

(deftest format.^.\:@{.39
  (format nil "~:@{~v,1,v^~A~}" '(#\a nil 0))
  "0")

;;; ~:^ in ~:{

(deftest format.\:^.\:{.1
  (format nil "~:{~:^~A~}"  nil)
  "")

(deftest format.\:^.\:{.2
  (format nil "(~:{~A~:^,~})"  '((1)(2)(3)))
  "(1,2,3)")

(deftest format.\:^.\:{.3
  (format nil "~:{~:^~A~}"  '((1)(2)(3)(4)))
  "123")

;;; arguments

(deftest format.\:^.\:{.4
  (format nil "~:{~0:^~A~}" '((1)(2)))
  "")

(deftest format.\:^.\:{.5
  (format nil "~:{~1:^~A~}" '((1)(2)))
  "12")

(deftest format.\:^.\:{.6
  (format nil "~:{~'X:^~A~}" '((1)(2)))
  "12")

(deftest format.\:^.\:{.7
  (format nil "~:{~v:^~A~}" '((1 8)(2 3 4)(3 1)(0)(6 7)(8 10)))
  "831")

(deftest format.\:^.\:{.8
  (format nil "~:{~V:^~A~}" '((#\X 1)(0 2)))
  "1")

(deftest format.\:^.\:{.9
  (format nil "~:{~#:^~A~}" '((1)(2)(3 4)(5 6 7)()(8 9 10)))
  "1235")

(deftest format.\:^.\:{.10
  (format nil "~:{~1,1:^~A~}" '(()(1)(2 3)))
  "")

(deftest format.\:^.\:{.11
  (format nil "~:{~0,1:^~A~}" '((1)(2 3)))
  "12")

(deftest format.\:^.\:{.12
  (format nil "~:{~v,1:^~A~}" '((2 3)(4 5 6)(0 2)(1 7)(9 10)))
  "352")

(deftest format.\:^.\:{.13
  (format nil "~:{~1,V:^~A~}" '((2 3)(4 5 6)(0 2)(1 7)(9 10)))
  "352")

(deftest format.\:^.\:{.14
  (format nil "~:{~V,v:^~A~}" '((0 1 2) (1 0 3) (4 4) () (5 6 7)))
  "23")

(deftest format.\:^.\:{.15
  (format nil "~:{~#,1:^~A~}" '((2 3 4)(4 5)(0)(1 7)(9 10)))
  "24")

(deftest format.\:^.\:{.16
  (format nil "~:{~1,#:^~A~}" '((2 3 4)(4 5)(0)(1 7)(9 10)))
  "24")

(deftest format.\:^.\:{.17
  (format nil "~:{~#,#:^~A~}" '(nil))
  "")

(deftest format.\:^.\:{.18
  (format nil "~:{~#,#:^~A~}" '((1)))
  "")

(deftest format.\:^.\:{.19
  (format nil "~:{~#,v:^~A~}" '((1 2)(3 4)(2 5 6)(1)(2)))
  "245")

(deftest format.\:^.\:{.20
  (format nil "~:{~V,#:^~A~}" '((0 2)(1 3 4)(1 3)()(0 7)))
  "23")

(deftest format.\:^.\:{.21
  (format nil "~:{~'X,'Y:^~A~}" '((1)(2)))
  "12")

(deftest format.\:^.\:{.22
  (format nil "~:{~'X,'X:^~A~}" '((1)(2)))
  "")

(deftest  format.\:^.\:{.23
  (format nil "~:{~1,2,3:^~A~}" '((1)(2)))
  "")

(deftest  format.\:^.\:{.24
  (format nil "~:{~1,2,1:^~A~}" '((1)(2)))
  "12")

(deftest  format.\:^.\:{.25
  (format nil "~:{~2,1,3:^~A~}" '((1)(2)))
  "12")

(deftest  format.\:^.\:{.26
  (format nil "~:{~1,1,v:^~A~}" '((0 4)(nil 1)(0 5)))
  "4")

(deftest  format.\:^.\:{.27
  (format nil "~:{~v,2,2:^~A~}" '((3 4)(1 1)(4 5)))
  "4")

(deftest  format.\:^.\:{.28
  (format nil "~:{~1,v,2:^~A~}" '((0 2)(3 4)(1 1)(4 5)))
  "24")

(deftest  format.\:^.\:{.29
  (format nil "~:{~V,v,3:^~A~}" '((1 4 0)(2 1 7)(4 4 8 0)(1 2 6)(9 8 0)))
  "078")

(deftest  format.\:^.\:{.30
  (format nil "~:{~v,2,v:^~A~}" '((1 1 0)(3 2 5)(2 1 6)(1 2 0)(10 11 13)))
  "056")

(deftest  format.\:^.\:{.31
  (format nil "~:{~2,V,v:^~A~}" '((1 1 0)(3 2 5)(2 1 6)(10 11 13)(0 1 0)))
  "056")

(deftest  format.\:^.\:{.32
  (format nil "~:{~v,v,V:^~A~}" '((1 2 1 0)(2 1 1 4)(2 3 1 6)(1 2 3)(0 1 0 8)))
  "046")

(deftest  format.\:^.\:{.33
  (format nil "~:{~#,2,2:^~A~}" '((1 2 3)(2 X X)(0 A B C D)(4 5)(5 7 8 9)))
  "120")

(deftest  format.\:^.\:{.34
  (format nil "~:{~2,#,3:^~A~}" '((1)(2 3 4 5)(3 4)(4 5 6 7 8)()))
  "12")

(deftest  format.\:^.\:{.35
  (format nil "~:{~1,3,#:^~A~}" '((1)(2 3)(3 4)(4 5 6)(5)))
  "123")

(deftest  format.\:^.\:{.36
  (format nil "~:{~#,#,2:^~A~}" '((1 2 3)(2 X X)(0 A B C D)(4 5)(5 7 8 9)))
  "120")

(deftest  format.\:^.\:{.37
  (format nil "~:{~3,#,#:^~A~}" '((1)(2 3)(3 4)(4 5 6)(5)))
  "123")

(deftest  format.\:^.\:{.38
  (format nil "~:{~#,2,#:^~A~}" '((1 2 3)(2)(0 A B C D)(4 5)(5 7 8 9)))
  "120")

(deftest  format.\:^.\:{.39
  (format nil "~:{~#,#,#:^~A~}" '((1 2 3)(2)(0 A B C D)(4 5)(5 7 8 9)))
  "")

;;; ~:^ in ~:@{

(deftest format.\:^.\:@{.1
  (format nil "~:@{~:^~A~}")
  "")

(deftest format.\:^.\:@{.2
  (format nil "(~:@{~A~:^,~})" '(1) '(2) '(3))
  "(1,2,3)")

(deftest format.\:^.\:@{.3
  (format nil "~:@{~:^~A~}" '(1) '(2) '(3) '(4))
  "123")

(deftest format.\:^.\:@{.4
  (format nil "~:@{~0:^~A~}" '(1) '(2))
  "")

(deftest format.\:^.\:@{.5
  (format nil "~:@{~1:^~A~}" '(1) '(2))
  "12")

(deftest format.\:^.\:@{.6
  (format nil "~:@{~'X:^~A~}" '(1) '(2))
  "12")

(deftest format.\:^.\:@{.7
  (format nil "~:@{~v:^~A~}" '(1 8) '(2 3 4) '(3 1) '(0) '(6 7) '(8 10))
  "831")

(deftest format.\:^.\:@{.8
  (format nil "~:@{~V:^~A~}" '(#\X 1) '(0 2))
  "1")

(deftest format.\:^.\:@{.9
  (format nil "~:@{~#:^~A~}" '(1) '(2) '(3 4) '(5 6 7) () '(8 9 10))
  "1235")

(deftest format.\:^.\:@{.10
  (format nil "~:@{~1,1:^~A~}" () '(1) '(2 3))
  "")

(deftest format.\:^.\:@{.11
  (format nil "~:@{~0,1:^~A~}" '(1) '(2 3))
  "12")

(deftest format.\:^.\:@{.12
  (format nil "~:@{~v,1:^~A~}" '(2 3) '(4 5 6) '(0 2) '(1 7) '(9 10))
  "352")

(deftest format.\:^.\:@{.13
  (format nil "~:@{~1,V:^~A~}" '(2 3) '(4 5 6) '(0 2) '(1 7) '(9 10))
  "352")

(deftest format.\:^.\:@{.14
  (format nil "~:@{~V,v:^~A~}" '(0 1 2) '(1 0 3) '(4 4) () '(5 6 7))
  "23")

(deftest format.\:^.\:@{.15
  (format nil "~:@{~#,1:^~A~}" '(2 3 4) '(4 5) '(0) '(1 7) '(9 10))
  "24")

(deftest format.\:^.\:@{.16
  (format nil "~:@{~1,#:^~A~}" '(2 3 4) '(4 5) '(0) '(1 7) '(9 10))
  "24")

(deftest format.\:^.\:@{.17
  (format nil "~:@{~#,#:^~A~}" nil)
  "")

(deftest format.\:^.\:@{.18
  (format nil "~:@{~#,#:^~A~}" '(1))
  "")

(deftest format.\:^.\:@{.19
  (format nil "~:@{~#,v:^~A~}" '(1 2) '(3 4) '(2 5 6) '(1) '(2))
  "245")

(deftest format.\:^.\:@{.20
  (format nil "~:@{~V,#:^~A~}" '(0 2) '(1 3 4) '(1 3) () '(0 7))
  "23")

(deftest format.\:^.\:@{.21
  (format nil "~:@{~'X,'Y:^~A~}" '(1) '(2))
  "12")

(deftest format.\:^.\:@{.22
  (format nil "~:@{~'X,'X:^~A~}" '(1) '(2))
  "")

(deftest  format.\:^.\:@{.23
  (format nil "~:@{~1,2,3:^~A~}" '(1) '(2))
  "")

(deftest  format.\:^.\:@{.24
  (format nil "~:@{~1,2,1:^~A~}" '(1) '(2))
  "12")

(deftest  format.\:^.\:@{.25
  (format nil "~:@{~2,1,3:^~A~}" '(1) '(2))
  "12")

(deftest  format.\:^.\:@{.26
  (format nil "~:@{~1,1,v:^~A~}" '(0 4) '(nil 1) '(0 5))
  "4")

(deftest  format.\:^.\:@{.27
  (format nil "~:@{~v,2,2:^~A~}" '(3 4) '(1 1) '(4 5))
  "4")

(deftest  format.\:^.\:@{.28
  (format nil "~:@{~1,v,2:^~A~}" '(0 2) '(3 4) '(1 1) '(4 5))
  "24")

(deftest  format.\:^.\:@{.29
  (format nil "~:@{~V,v,3:^~A~}" '(1 4 0) '(2 1 7) '(4 4 8 0) '(1 2 6) '(9 8 0))
  "078")

(deftest  format.\:^.\:@{.30
  (format nil "~:@{~v,2,v:^~A~}" '(1 1 0) '(3 2 5) '(2 1 6) '(1 2 0) '(10 11 13))
  "056")

(deftest  format.\:^.\:@{.31
  (format nil "~:@{~2,V,v:^~A~}" '(1 1 0) '(3 2 5) '(2 1 6) '(10 11 13) '(0 1 0))
  "056")

(deftest  format.\:^.\:@{.32
  (format nil "~:@{~v,v,V:^~A~}" '(1 2 1 0) '(2 1 1 4) '(2 3 1 6) '(1 2 3) '(0 1 0 8))
  "046")

(deftest  format.\:^.\:@{.33
  (format nil "~:@{~#,2,2:^~A~}" '(1 2 3) '(2 X X) '(0 A B C D) '(4 5) '(5 7 8 9))
  "120")

(deftest  format.\:^.\:@{.34
  (format nil "~:@{~2,#,3:^~A~}" '(1) '(2 3 4 5) '(3 4) '(4 5 6 7 8) ())
  "12")

(deftest  format.\:^.\:@{.35
  (format nil "~:@{~1,3,#:^~A~}" '(1) '(2 3) '(3 4) '(4 5 6) '(5))
  "123")

(deftest  format.\:^.\:@{.36
  (format nil "~:@{~#,#,2:^~A~}" '(1 2 3) '(2 X X) '(0 A B C D) '(4 5) '(5 7 8 9))
  "120")

(deftest  format.\:^.\:@{.37
  (format nil "~:@{~3,#,#:^~A~}" '(1) '(2 3) '(3 4) '(4 5 6) '(5))
  "123")

(deftest  format.\:^.\:@{.38
  (format nil "~:@{~#,2,#:^~A~}" '(1 2 3) '(2) '(0 A B C D) '(4 5) '(5 7 8 9))
  "120")

(deftest  format.\:^.\:@{.39
  (format nil "~:@{~#,#,#:^~A~}" '(1 2 3) '(2) '(0 A B C D) '(4 5) '(5 7 8 9))
  "")
