;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 25 19:27:25 2004
;;;; Contains: Tests of the ~[ ~] forms

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest format.cond.1
  (format nil "~[~]" 0)
  "")

(deftest format.cond.2
  (format nil "~[a~]" 0)
  "a")

(deftest format.cond.3
  (format nil "~[a~]" -1)
  "")

(deftest format.cond.4
  (format nil "~[a~]" (1- most-negative-fixnum))
  "")

(deftest format.cond.5
  (format nil "~[a~]" 1)
  "")

(deftest format.cond.6
  (format nil "~[a~]" (1+ most-positive-fixnum))
  "")

(deftest format.cond.7
  (loop for i from -1 to 10
	collect (format nil "~[a~;b~;c~;d~;e~;f~;g~;h~;i~]" i))
  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

(deftest format.cond.8
  (format nil "~0[a~;b~;c~;d~]" 3)
  "a")

(deftest format.cond.9
  (format nil "~-1[a~;b~;c~;d~]" 3)
  "")

(deftest format.cond.10
  (format nil "~1[a~;b~;c~;d~]" 3)
  "b")

(deftest format.cond.11
  (format nil "~4[a~;b~;c~;d~]" 3)
  "")

(deftest format.cond.12
  (format nil "~100000000000000000000000000000000[a~;b~;c~;d~]" 3)
  "")

(deftest format.cond.13
  (loop for i from -1 to 10
	collect (format nil "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]" i nil))
  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

(deftest format.cond.14
  (loop for i from -1 to 10
	collect (format nil "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]" nil i))
  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

(deftest format.cond.15
  (format nil "~#[A~;B~]")
  "A")

(deftest format.cond.16
  (format nil "~#[A~;B~]" nil)
  "B")

;;; ~[ .~:;  ~]

(deftest format.cond\:.1
  (loop for i from -100 to 100
	for s = (format nil "~[~:;a~]" i)
	unless (or (zerop i) (string= s "a"))
	collect (list i s))
  nil)

(deftest format.cond\:.2
  (format nil "~[a~:;b~]" 0)
  "a")

(deftest format.cond\:.3
  (format nil "~[a~:;b~]" (1- most-negative-fixnum))
  "b")

(deftest format.cond\:.4
  (format nil "~[a~:;b~]" (1+ most-positive-fixnum))
  "b")

(deftest format.cond\:.5
  (loop for i from -1 to 10
	collect (format nil "~[a~;b~;c~;d~:;e~]" i))
  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

(deftest format.cond\:.6
  (loop for i from -1 to 10
	collect (format nil "~v[a~;b~;c~;d~:;e~]" i nil))
  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

(deftest format.cond\:.7
  (loop for i from -1 to 10
	collect (format nil "~v[a~;b~;c~;d~:;e~]" nil i))
  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

(deftest format.cond\:.8
  (format nil "~#[A~:;B~]")
  "A")

(deftest format.cond\:.9
  (format nil "~#[A~:;B~]" nil nil)
  "B")

;;; ~:[...~]

(deftest format.\:cond.1
  (format nil "~:[a~;b~]" nil)
  "a")

(deftest format.\:cond.2
  (loop for x in *mini-universe*
	for s = (format nil "~:[a~;b~]" x)
	when (and x (not (string= s "b")))
	collect (list x s))
  nil)

;;; ~@[ ... ~]

(deftest format.@cond.1
  (format nil "~@[X~]Y~A" 1)
  "XY1")

(deftest format.@cond.2
  (format nil "~@[X~]Y~A" nil 2)
  "Y2")
