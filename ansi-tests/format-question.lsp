;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct 17 20:08:18 2004
;;;; Contains: Tests of the ~? and ~@? format directives

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest format.?.1
  (format nil "~?" "" nil)
  "")

(deftest format.?.2
  (format nil "~?" "~A" '(1))
  "1")

(deftest format.?.3
  (format nil "~?" "" '(1))
  "")

(deftest format.?.4
  (format nil "~? ~A" "" '(1) 2)
  " 2")

(deftest format.?.5
  (format nil "a~?z" "b~?y" '("c~?x" ("~A" (1))))
  "abc1xyz")

;;; Tests of ~@?

(deftest format.@?.1
  (format nil "~@?" "")
  "")

(deftest format.@?.2
  (format nil "~@?" "~A" 1)
  "1")

(deftest format.@?.3
  (format nil "~@? ~A" "<~A>" 1 2)
  "<1> 2")

(deftest format.@?.4
  (format nil "a~@?z" "b~@?y" "c~@?x" "~A" 1)
  "abc1xyz")

(deftest format.@?.5
  (format nil "~{~A~@?~A~}" '(1 "~4*" 2 3 4 5 6))
  "16")
