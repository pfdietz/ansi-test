;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan 13 16:32:44 2003
;;;; Contains: Tests of COMPILED-FUNCTION-P

(in-package :cl-test)

(deftest compiled-function-p.1
  (some #'(lambda (obj)
	     (if (check-values (compiled-function-p obj))
		 (not (typep obj 'compiled-function))
	       (typep obj 'compiled-function)))
	 *universe*)
  nil)

(deftest compiled-function-p.2
  (compiled-function-p '(lambda (x y) (cons y x)))
  nil)

(deftest compiled-function-p.3
  (notnot-mv (compiled-function-p (compile nil '(lambda (y x) (cons x y)))))
  t)

(deftest compiled-function-p.order.1
  (let ((i 0))
    (values
     (compiled-function-p (progn (incf i) '(lambda () nil)))
     i))
  nil 1)

(deftest compiled-function-p.error.1
  (classify-error (compiled-function-p))
  program-error)

(deftest compiled-function-p.error.2
  (classify-error (compiled-function-p nil nil))
  program-error)


