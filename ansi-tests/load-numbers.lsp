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
(load "round.lsp")
(load "fround.lsp")

;;; transcendental functions go here

(load "times.lsp")
(load "plus.lsp")
(load "minus.lsp")
(load "divide.lsp")
(load "oneplus.lsp")
(load "oneminus.lsp")
(load "abs.lsp")
;;(load "exp.lsp")
(load "expt.lsp")
(load "gcd.lsp")
(load "incf.lsp")
(load "decf.lsp")
(load "lcm.lsp")
(load "signum.lsp")
(load "sqrt.lsp")
(load "isqrt.lsp")
(load "random.lsp")
(load "random-state-p.lsp")
(load "make-random-state.lsp")
(load "numberp.lsp")
(load "cis.lsp")
(load "complex.lsp")
(load "complexp.lsp")
(load "conjugate.lsp")
(load "phase.lsp")
(load "realpart.lsp")
(load "imagpart.lsp")
(load "realp.lsp")
(load "numerator-denominator.lsp")
(load "rationalp.lsp")

(load "ash.lsp")
(load "integer-length.lsp")
(load "integerp.lsp")

(load "parse-integer.lsp")
(load "boole.lsp")

(load "rational.lsp")
(load "rationalize.lsp")

(load "evenp.lsp")
(load "oddp.lsp")

(load "epsilons.lsp")
