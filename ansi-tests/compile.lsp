;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Oct 10 20:54:20 2002
;;;; Contains: Tests for COMPILE, COMPILED-FUNCTION-P, COMPILED-FUNCTION

(in-package :cl-test)

(deftest compiled-function-p.1
  (some #'(lambda (obj)
	     (if (compiled-function-p obj)
		 (not (typep obj 'compiled-function))
	       (typep obj 'compiled-function)))
	 *universe*)
  nil)

(deftest compiled-function-p.2
  (compiled-function-p '(lambda (x y) (cons y x)))
  nil)

(deftest compiled-function-p.3
  (not (compiled-function-p (compile nil '(lambda (y x) (cons x y)))))
  nil)

(deftest compile.1
  (progn
    (fmakunbound 'compile.1-fn)
    (values
     (defun compile.1-fn (x) x)
     (compiled-function-p 'compile.1-fn)
     (compile 'compile.1-fn)
     (compiled-function-p 'compile.1-fn)
     (not (compiled-function-p #'compile.1-fn))
     (fmakunbound 'compile.1-fn)))
  compile.1-fn
  nil
  compile.1-fn
  nil
  nil
  compile.1-fn)


;;; COMPILE returns three values (function, warnings-p, failure-p)
(deftest compile.2
  (let* ((results (multiple-value-list
		   (compile nil '(lambda (x y) (cons y x)))))
	 (fn (car results)))
    (values (length results)
	    (funcall fn 'a 'b)
	    (second results)
	    (third results)))
  3
  (b . a)
  nil
  nil)

;;; Compile does not coalesce literal constants
(deftest compile.3
  (let ((x (list 'a 'b))
	(y (list 'a 'b)))
    (and (not (eq x y))
	 (funcall (compile nil `(lambda () (eq ',x ',y))))))
  nil)

(deftest compile.4
  (let ((x (copy-seq "abc"))
	(y (copy-seq "abc")))
    (and (not (eq x y))
	 (funcall (compile nil `(lambda () (eq ,x ,y))))))
  nil)

(deftest compile.5
  (let ((x (copy-seq "abc")))
    (funcall (compile nil `(lambda () (eq ,x ,x)))))
  t)

(deftest compile.6
  (let ((x (copy-seq "abc")))
    (funcall (compile nil `(lambda () (eq ',x ',x)))))
  t)

(deftest compile.7
  (let ((x (copy-seq "abc")))
    (eq x (funcall (compile nil `(lambda () ,x)))))
  t)

(deftest compile.8
  (let ((x (list 'a 'b)))
    (eq x (funcall (compile nil `(lambda () ',x)))))
  t)
