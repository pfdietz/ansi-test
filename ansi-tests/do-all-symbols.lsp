;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Feb 21 18:27:22 2004
;;;; Contains: Tests of DO-ALL-SYMBOLS

(in-package :cl-test)

(def-macro-test do-all-symbols.error.1
  (do-all-symbols (x)))

;;; FIXME  Add tests for non-error cases
