;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 22 06:53:55 2004
;;;; Contains: Tests of the KEYWORD package

(in-package :cl-test)

;; Check that each keyword satisfies keywordp

(deftest keyword.1
  (do-symbols (s "KEYWORD" t)
    (unless (keywordp s)
      (return (list s nil))))
  t)

;; Every keyword is external
(deftest keyword.2
  (do-symbols (s "KEYWORD" t)
    (multiple-value-bind (s2 access)
	(find-symbol (symbol-name s) "KEYWORD")
      (unless (and (eqt s s2)
		   (eqt access :external))
	(return (list s2 access)))))
  t)

;; Every keyword evaluates to itself
(deftest keyword.3
  (do-symbols (s "KEYWORD" t)
    (unless (eqt s (eval s))
      (return (list s (eval s)))))
  t)

