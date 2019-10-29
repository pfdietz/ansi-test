;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 20 11:50:26 2005
;;;; Contains: Randomized tests of type propagation during compilation

(compile-and-load "random-type-prop.lsp")

(in-package :cl-test)

(loop for i from 1 to 17
   do (load (format nil "random/random-type-prop-tests-~2,vD.lsp" #\0 i)))

(load "random/random-type-prop-tests-structs.lsp")

