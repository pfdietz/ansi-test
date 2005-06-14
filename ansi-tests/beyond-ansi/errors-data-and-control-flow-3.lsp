;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jun 14 07:00:58 2005
;;;; Contains: Tests of non-ANSI exceptions sutation from CLHS section 5, part 3
(in-package :ba-test)

(compile-and-load "ba-aux.lsp")

;;; CASE

(def-error-test case.1 (case . 1))
(def-error-test case.2 (case nil . 1))
(def-error-test case.3 (case nil (nil . 1)))
(def-error-test case.4 (case 'x nil))
(def-error-test case.5 (case 'x ((nil . x) 1)))
(def-error-test case.6 (case))

;;; CCASE

(def-error-test ccase.1 (ccase . 1))
(def-error-test ccase.2 (let ((x nil)) (ccase x . 1)))
(def-error-test ccase.3 (let ((x nil)) (ccase x (nil . 1))))
(def-error-test ccase.4 (let ((x 'x)) (ccase x nil)))
(def-error-test ccase.5 (let ((x 'x)) (ccase x ((nil . x) 1))))
(def-error-test ccase.6 (ccase 1 (1 nil))) ;; 1 is not a place!
(def-error-test ccase.7 (ccase))

;;; ECASE

(def-error-test ecase.1 (ecase . 1))
(def-error-test ecase.2 (ecase nil . 1))
(def-error-test ecase.3 (ecase nil (nil . 1)))
(def-error-test ecase.4 (ecase 'x nil))
(def-error-test ecase.5 (ecase 'x ((nil . x) 1)))
(def-error-test ecase.6 (ecase))

;;; TYPECASE

(def-error-test typecase.1 (typecase))
(def-error-test typecase.2 (typecase . :foo))
(def-error-test typecase.3 (typecase 'x . #\X))
(def-error-test typecase.4 (typecase 'x (#.(gensym) t)))
(def-error-test typecase.5 (typecase 'x (symbol . :foo)))
(def-error-test typecase.6 (typecase 'x . :foo))
(def-error-test typecase.7 (typepcase 'x (t . :foo)))
(def-error-test typecase.8 (typepcase 'x (otherwise . :foo)))

;;; CTYPECASE

(def-error-test ctypecase.1 (ctypecase))
(def-error-test ctypecase.2 (ctypecase . :foo))
(def-error-test ctypecase.3 (let ((x 'x)) (ctypecase x . #\X)))
(def-error-test ctypecase.4 (let ((x 'x)) (ctypecase x (#.(gensym) t))))
(def-error-test ctypecase.5 (let ((x 'x)) (ctypecase x (symbol . :foo))))
(def-error-test ctypecase.6 (let ((x 'x)) (ctypecase x . :foo)))
(def-error-test ctypecase.7 (let ((x 'x)) (ctypecase x (t . :foo))))
(def-error-test ctypecase.8 (let ((x 'x)) (ctypecase x (otherwise . :foo))))
(def-error-test ctypecase.9 (ctypecase 1 (integer :bad)))

;;; ETYPECASE

(def-error-test etypecase.1 (etypecase))
(def-error-test etypecase.2 (etypecase . :foo))
(def-error-test etypecase.3 (etypecase 'x . #\X))
(def-error-test etypecase.4 (etypecase 'x (#.(gensym) t)))
(def-error-test etypecase.5 (etypecase 'x (symbol . :foo)))
(def-error-test etypecase.6 (etypecase 'x . :foo))

;;; MULTIPLE-VALUE-BIND

(def-error-test multiple-value-bind.1 (multiple-value-bind))
(def-error-test multiple-value-bind.2 (multiple-value-bind .
					  #.(1+ most-positive-fixnum)))
(def-error-test multiple-value-bind.3 (multiple-value-bind (x)))
(def-error-test multiple-value-bind.4 (multiple-value-bind (x . y) 1 x))
(def-error-test multiple-value-bind.5 (multiple-value-bind (x) . :foo))
(def-error-test multiple-value-bind.6 (multiple-value-bind (x) nil . :bar))
(def-error-test multiple-value-bind.7
  (multiple-value-bind (x) nil "doc string" . 1))
(def-error-test multiple-value-bind.8
  (multiple-value-bind (x) nil (declare) . 1))
(def-error-test multiple-value-bind.9
  (multiple-value-bind (x) 1 (declare (type symbol x)) x))

;;; MULTIPLE-VALUE-CALL

(def-error-test multiple-value-call.1 (multiple-value-call))
(def-error-test multiple-value-call.2 (multiple-value-call . :x))
(def-error-test multiple-value-call.3 (multiple-value-call 'list . :x))
(def-error-test multiple-value-call.4 (multiple-value-call 'list 1 . :x))
(def-all-error-test multiple-value-call.5 'function-designator-p
  '(multiple-value-call x nil))
(def-error-test multiple-value-call.6 (multiple-value-call (gensym)))

;;; MULTIPLE-VALUE-LIST

(def-error-test multiple-value-list.1 (multiple-value-list))
(def-error-test multiple-value-list.2 (multiple-value-list . 1))
(def-error-test multiple-value-list.3 (multiple-value-list 1 . 2))
(def-error-test multiple-value-list.4 (multiple-value-list 1 2))

;;; MULTIPLE-VALUE-PROG1

(def-error-test multiple-value-prog1.1 (multiple-value-prog1))
(def-error-test multiple-value-prog1.2 (multiple-value-prog1 . 1))
(def-error-test multiple-value-prog1.3 (multiple-value-prog1 :x . :y))

;;; MULTIPLE-VALUE-SETQ

(def-error-test multiple-value-setq.1 (multiple-value-setq))
(def-error-test multiple-value-setq.2 (let (x) (multiple-value-setq (x)) x))
(def-error-test multiple-value-setq.3
  (let (x y) (multiple-value-setq (x . y) nil (list x y))))
(def-all-error-test multiple-value-setq.4 'symbolp
  #'(lambda (x) `(multiple-value-setq (,x) nil)))
(def-all-error-test multiple-value-setq.5 (constantly nil)
  #'(lambda (x) `(multiple-value-setq (,x) nil))
  :vals cl-test::*cl-constant-symbols*)
