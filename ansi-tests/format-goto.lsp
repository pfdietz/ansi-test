;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug 24 06:56:13 2004
;;;; Contains: Tests of the ~* format directive

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

;;; ~*

(deftest format.*.1
  (format nil "~A~*~A" 1 2 3)
  "13")

(deftest format.*.2
  (format nil "~A~0*~A" 1 2 3)
  "12")

(deftest format.*.3
  (format nil "~A~v*~A" 1 0 2)
  "12")

(deftest format.*.4
  (format nil "~A~v*~A" 1 1 2 3)
  "13")

(deftest format.*.5
  (format nil "~A~v*~A" 1 nil 2 3)
  "13")

(deftest format.*.6
  (format nil "~A~1{~A~*~A~}~A" 0 '(1 2 3) 4)
  "0134")

(deftest format.*.7
  (format nil "~A~1{~A~0*~A~}~A" 0 '(1 2 3) 4)
  "0124")

(deftest format.*.8
  (format nil "~A~{~A~*~A~}~A" 0 '(1 2 3 4 5 6) 7)
  "013467")

;;; ~:*

(deftest format.\:*.1
  (format nil "~A~:*~A" 1 2 3)
  "11")

(deftest format.\:*.2
  (format nil "~A~A~:*~A" 1 2 3)
  "122")

(deftest format.\:*.3
  (format nil "~A~A~0:*~A" 1 2 3)
  "123")

(deftest format.\:*.4
  (format nil "~A~A~2:*~A" 1 2 3)
  "121")

(deftest format.\:*.5
  (format nil "~A~A~v:*~A" 1 2 0 3)
  "123")

(deftest format.\:*.6
  (format nil "~A~A~v:*~A" 6 7 2 3)
  "677")

(def-pprint-test format.\:*.7
  (format nil "~A~A~v:*~A" 6 7 nil 3)
  "67NIL")

;;; ~@*

(deftest format.@*.1
  (format nil "~A~A~@*~A~A" 1 2 3 4)
  "1212")

(deftest format.@*.2
  (format nil "~A~A~1@*~A~A" 1 2 3 4)
  "1223")

(deftest format.@*.3
  (format nil "~A~A~2@*~A~A" 1 2 3 4)
  "1234")

(deftest format.@*.4
  (format nil "~A~A~3@*~A~A" 1 2 3 4 5)
  "1245")

(deftest format.@*.5
  (format nil "~A~A~v@*~A~A" 1 2 nil 3 4)
  "1212")

(deftest format.@*.6
  (format nil "~A~A~v@*~A~A" 1 2 1 3 4)
  "1221")

(deftest format.@*.7
  (format nil "~A~A~v@*~A~A" 6 7 2 3 4)
  "6723")


