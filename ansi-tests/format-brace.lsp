;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 25 22:08:51 2004
;;;; Contains: Tests of the ~"{ ... ~} format directives

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest format.{.1
  (format nil "~{~
~}" nil)
  "")

(deftest format.{.1a
  (format nil "~{~}" "" nil)
  "")

(deftest format.{.1b
  (format nil "~0{~}" "" '(1 2 3))
  "")

(deftest format.{.2
  (format nil "~{ ~}" nil)
  "")

(deftest format.{.3
  (format nil "~{X Y Z~}" nil)
  "")

(deftest format.{.4
  (format nil "~{~A~}" '(1 2 3 4))
  "1234")

(deftest format.{.5
  (format nil "~{~{~A~}~}" '((1 2 3)(4 5)(6 7 8)))
  "12345678")

(deftest format.{.6
  (format nil "~{~1{~A~}~}" '((1 2 3)(4 5)(6 7 8)))
  "146")

(deftest format.{.7
  (format nil "~1{~
~}" nil)
  "")

(deftest format.{.8
  (loop for i from 0 to 10
	for s = (format nil "~v{~A~}" i '(1 2 3 4 5 6 7 8 9 0))
	unless (string= s (subseq "1234567890" 0 i))
	collect (list i s))
  nil)

(deftest format.{.9
  (format nil "~#{~A~}" '(1 2 3 4 5 6 7) nil nil nil)
  "1234")

;;; Tests of ~^ inside ~{ ... ~}

(deftest format.{.10
  (format nil "~{X ~A~^ Y ~A~^ ~}" '(1 2 3 4 5))
  "X 1 Y 2 X 3 Y 4 X 5")

(deftest format.{.11
  (format nil "~{X ~A~^ Y ~A~^ ~}" '(1 2 3 4))
  "X 1 Y 2 X 3 Y 4")

(deftest format.{.12
  (format nil "~1{~A~^~A~}" '(1))
  "1")

(deftest format.{.13
  (format nil "~0{~A~^~A~}" '(1))
  "")

(deftest format.{.14
  (format nil "~1{~A~^~A~}" '(1 2 3))
  "12")

(deftest format.{.15
  (format nil "~0{~}" "~A" '(1 2 3))
  "")

(deftest format.{.16
  (format nil "~1{~}" "~A" '(4 5 6))
  "4")

(deftest format.{.17
  (format nil "~{~}" (formatter "") nil)
  "")

(deftest format.{.18
  (format nil "~1{~}" (formatter "") '(1 2 3 4))
  "")

(deftest format.{.19
  (format nil "~{~}" (formatter "~A") '(1 2 3 4))
  "1234")

(deftest format.{.20
  (format nil "~3{~}" (formatter "~A") '(1 2 3 4))
  "123")

(deftest format.{.21
  (format nil "~V{~}" 2 "~A" '(1 2 3 4 5))
  "12")

(deftest format.{.22
  (format nil "~#{~}" "~A" '(1 2 3 4 5))
  "12")

(deftest format.{.23
  (format nil "~{FOO~:}" nil)
  "FOO")

(deftest format.{.24
  (format nil "~{~A~:}" '(1))
  "1")

(deftest format.{.25
  (format nil "~{~A~:}" '(1 2))
  "12")

(deftest format.{.26
  (format nil "~{~A~:}" '(1 2 3))
  "123")

(deftest format.{.27
  (format nil "~0{FOO~:}" nil)
  "")

(deftest format.{.28
  (format nil "~V{FOO~:}" 0 nil)
  "")

(deftest format.{.29
  (format nil "~1{FOO~:}" nil)
  "FOO")

(deftest format.{.30
  (format nil "~2{FOO~:}" nil)
  "FOO")

(deftest format.{.31
  (format nil "~2{~
~:}" nil)
  "")

(deftest format.{.32
  (format nil "~2{FOO~}" nil)
  "")

(deftest format.{.33
  (format nil "~v{~a~}" nil '(1 2 3 4 5 6 7))
  "1234567")

;;; ~:{ ... ~}

(deftest format.\:{.1
  (format nil "~:{(~A ~A)~}" '((1 2 3)(4 5)(6 7 8)))
  "(1 2)(4 5)(6 7)")

(deftest format.\:{.2
  (format nil "~:{~
~}" nil)
  "")

(deftest format.\:{.3
  (format nil "~:{~}" "" nil)
  "")

(deftest format.\:{.4
  (format nil "~:{~}" "~A" nil)
  "")

(deftest format.\:{.5
  (format nil "~:{~}" "X" '(nil (1 2) (3)))
  "XXX")

(deftest format.\:{.6
  (format nil "~:{~}" (formatter "~A") '((1 2) (3) (4 5 6)))
  "134")

(deftest format.\:{.7
  (format nil "~0:{XYZ~}" '((1)))
  "")
	  
(deftest format.\:{.8
  (format nil "~2:{XYZ~}" '((1)))
  "XYZ")
	  
(deftest format.\:{.9
  (format nil "~2:{~A~}" '((1) (2)))
  "12")
	  
(deftest format.\:{.10
  (format nil "~2:{~A~}" '((1 X) (2 Y) (3 Z)))
  "12")

(deftest format.\:{.11
  (loop for i from 0 to 10 collect
	(format nil "~v:{~A~}" i '((1) (2) (3 X) (4 Y Z) (5) (6))))
  ("" "1" "12" "123" "1234" "12345"
   "123456" "123456" "123456" "123456" "123456"))

(deftest format.\:{.12
  (format nil "~V:{X~}" nil '((1) (2) (3) nil (5)))
  "XXXXX")

(deftest format.\:{.13
  (format nil "~#:{~A~}" '((1) (2) (3) (4) (5)) 'foo 'bar)
  "123")

(deftest format.\:{.14
  (format nil "~:{~A~:}" '((1 X) (2 Y) (3) (4 A B)))
  "1234")

(deftest format.\:{.15
  (loop for i from 0 to 10 collect
	(format nil "~v:{~A~:}" i '((1 X) (2 Y) (3) (4 A B))))
  ("" "1" "12" "123" "1234" "1234"
   "1234" "1234" "1234" "1234" "1234"))

(deftest format.\:{.16
  (format nil "~:{ABC~:}" '(nil))
  "ABC")

(deftest format.\:{.17
  (format nil "~v:{ABC~:}" nil '(nil))
  "ABC")

;;; ~^ in ~:{ ... ~:}



;;; Tests of ~@{ ... ~}

(deftest format.@{.1
  (format nil "~@{~
~}")
  "")

(deftest format.@{.1A
  (format nil "~@{~}" "")
  "")

(deftest format.@{.2
  (format nil "~@{ ~}")
  "")

(deftest format.@{.3
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (format nil "~@{X ~A Y Z~}" nil)))
  "X NIL Y Z")

(deftest format.@{.4
  (format nil "~@{~A~}" 1 2 3 4)
  "1234")

(deftest format.@{.5
  (format nil "~@{~{~A~}~}" '(1 2 3) '(4 5) '(6 7 8))
  "12345678")

(deftest format.@{.6
  (format nil "~@{~1{~A~}~}" '(1 2 3) '(4 5) '(6 7 8))
  "146")

(deftest format.@{.7
  (format nil "~1@{FOO~}")
  "")

(deftest format.@{.8
  (format nil "~v@{~A~}" nil 1 4 7)
  "147")

(deftest format.@{.9
  (format nil "~#@{~A~}" 1 2 3)
  "123")

(deftest format.@{.10
  (loop for i from 0 to 10
	for x = nil then (cons i x)
	collect (apply #'format nil "~v@{~A~}" i (reverse x)))
  ("" "1" "12" "123" "1234" "12345"
   "123456" "1234567" "12345678" "123456789" "12345678910"))

(deftest format.@{.11
  (format nil "~@{X~:}")
  "X")

(deftest format.@{.12
  (format nil "~@{~}" (formatter "X~AY") 1)
  "X1Y")

(deftest format.@{.13
  (format nil "~v@{~}" 1 (formatter "X") 'foo)
  "X")

;;; ~:@{

(deftest format.\:@{.1
  (format nil "~:@{~
~}")
  "")

(deftest format.\:@{.2
  (format nil "~:@{~A~}" '(1 2) '(3) '(4 5 6))
  "134")

(deftest format.\:@{.3
  (format nil "~:@{(~A ~A)~}" '(1 2 4) '(3 7) '(4 5 6))
  "(1 2)(3 7)(4 5)")

(deftest format.\:@{.4
  (format nil "~:@{~}" "(~A ~A)" '(1 2 4) '(3 7) '(4 5 6))
  "(1 2)(3 7)(4 5)")

(deftest format.\:@{.5
  (format nil "~:@{~}" (formatter "(~A ~A)") '(1 2 4) '(3 7) '(4 5 6))
  "(1 2)(3 7)(4 5)")

(deftest format.\:@.6
  (format nil "~:@{~A~:}" '(1 A) '(2 B) '(3) '(4 C D))
  "1234")

(deftest format.\:@.7
  (format nil "~0:@{~A~:}" '(1 A) '(2 B) '(3) '(4 C D))
  "")

(deftest format.\:@.8
  (format nil "~#:@{A~:}" nil nil nil)
  "AAA")

(deftest format.\:@.9
  (format nil "~v:@{~A~}" nil '(1) '(2) '(3))
  "123")

(deftest format.\:@.10
  (loop for i from 0 to 10
	for x = nil then (cons (list i) x)
	collect
	(apply #'format nil "~V:@{~A~}" i (reverse x)))
  ("" "1" "12" "123" "1234" "12345" "123456" "1234567" "12345678"
   "123456789" "12345678910"))


;;; Error tests

(deftest format.{.error.1
  (signals-error (format nil "~{~A~}" 'A) type-error)
  t)

(deftest format.{.error.2
  (signals-error (format nil "~{~A~}" 1) type-error)
  t)

(deftest format.{.error.3
  (signals-error (format nil "~{~A~}" "foo") type-error)
  t)

(deftest format.{.error.4
  (signals-error (format nil "~{~A~}" #*01101) type-error)
  t)

(deftest format.{.error.5
  (signals-error (format nil "~{~A~}" '(x y . z)) type-error)
  t)

(deftest format.\:{.error.1
  (signals-error (format nil "~:{~A~}" '(x)) type-error)
  t)

(deftest format.\:{.error.2
  (signals-error (format nil "~:{~A~}" 'x) type-error)
  t)
  
(deftest format.\:{.error.3
  (signals-error (format nil "~:{~A~}" '((x) . y)) type-error)
  t)

(deftest format.\:{.error.4
  (signals-error (format nil "~:{~A~}" '("X")) type-error)
  t)

(deftest format.\:{.error.5
  (signals-error (format nil "~:{~A~}" '(#(X Y Z))) type-error)
  t)

(deftest format.\:@.error.1
  (signals-error (format nil "~:@{~A~}" 'x) type-error)
  t)

(deftest format.\:@.error.2
  (signals-error (format nil "~:@{~A~}" 0) type-error)
  t)

(deftest format.\:@.error.3
  (signals-error (format nil "~:@{~A~}" #*01101) type-error)
  t)

(deftest format.\:@.error.4
  (signals-error (format nil "~:@{~A~}" "abc") type-error)
  t)

(deftest format.\:@.error.5
  (signals-error (format nil "~:@{~A ~A~}" '(x . y)) type-error)
  t)
