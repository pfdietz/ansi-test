;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Apr  7 07:16:44 2003
;;;; Contains: Forms to load files containing tests of number concepts

(in-package :cl-test)

(compile-and-load "numbers-aux.lsp")
(load "number-comparison.lsp")
(load "max.lsp")
(load "min.lsp")
(load "minusp.lsp")
(load "plusp.lsp")
(load "zerop.lsp")
(load "floor.lsp")
(load "ffloor.lsp")
(load "ceiling.lsp")
(load "fceiling.lsp")
(load "truncate.lsp")
(load "ftruncate.lsp")

(load "epsilons.lsp")
