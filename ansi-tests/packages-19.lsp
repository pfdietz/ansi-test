;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue May  5 17:22:49 1998
;;;; Contains: Packages test code, part 19.  Tests of the keyword package.
;;;;           See also cl-symbols.lsp (for keywordp test cases)

(in-package :cl-test)
(declaim (optimize (safety 3)))

;; Check that each keyword satisfies keywordp

(deftest keyword-1
    (do-symbols (s "KEYWORD" t)
      (unless (keywordp s)
	(return (list s nil))))
  t)

;; Every keyword is external
(deftest keyword-2
    (do-symbols (s "KEYWORD" t)
      (multiple-value-bind (s2 access)
	  (find-symbol (symbol-name s) "KEYWORD")
	(unless (and (eq s s2)
		     (eq access :external))
	  (return (list s2 access)))))
  t)

;; Every keyword evaluates to itself
(deftest keyword-3
    (do-symbols (s "KEYWORD" t)
      (unless (eq s (eval s))
	(return (list s (eval s)))))
  t)


