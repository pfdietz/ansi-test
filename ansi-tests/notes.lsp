;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jun 30 21:43:23 2003
;;;; Contains: Notes concerning various parts of the ANSI spec.

(in-package :cl-test)

(defnote :allow-nil-arrays
  "Allow specialized arrays of type (array nil).")

(defnote :allow-nonzero-nil-vectors
  "Allow specialized vectors of type (vector nil) of nonzero size.")

(defnote :nil-vectors-are-strings
  "Assume that (VECTOR NIL) objects are strings.")


