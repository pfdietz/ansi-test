;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 13 19:12:17 2003
;;;; Contains: Tests for CONSTANTP

;;; See also defconstant.lsp

(in-package :cl-test)

(deftest constantp.error.1
  (classify-error (constantp))
  program-error)

(deftest constantp.error.2
  (classify-error (constantp nil nil nil))
  program-error)


(deftest constantp.1
  (loop for e in *universe*
	when (and (not (symbolp e))
		   (not (consp e))
		   (not (constantp e)))
	collect e)
  nil)

(deftest constantp.2
  (notnot (constantp t))
  t)
  
(deftest constantp.3
  (notnot (constantp nil))
  t)

(deftest constantp.4
  (notnot (constantp :foo))
  t)

(deftest constantp.5
  (constantp (gensym))
  nil)

(defconstant constantp-test-symbol 1)

(defmacro constantp-macro (form &environment env)
  (notnot (constantp form env)))

(deftest constantp.6
  (constantp-macro constantp-test-symbol)
  t)
  
(deftest constantp.7
  (constantp '(incf x))
  nil)

(deftest constantp.8
  (notnot (constantp 1 nil))
  t)

(deftest constantp.9
  (notnot (constantp ''(((foo)))))
  t)


