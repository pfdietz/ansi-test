;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 14 10:13:21 1998
;;;; Contains: CL test case package definition

(defpackage :cl-test
  (:use :cl :regression-test)
  (:nicknames)
  (:shadow #:handler-case #:handler-bind)
  (:import-from "COMMON-LISP-USER" #:compile-and-load "==>")
  (:export #:random-from-seq #:random-case #:coin #:random-permute))

(let ((s (find-symbol "QUIT" "CL-USER")))
  (when s (import s :cl-test)))

