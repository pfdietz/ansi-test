;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:04:19 1998
;;;; Contains: Package test code, part 11

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unexport

(deftest unexport-1
    (handler-case
	(progn
	  (ignore-errors (delete-package "X"))
	  (let* ((p (make-package "X" :use nil))
		 (r (export (intern "X" p) p)))
	    (multiple-value-bind (sym1 access1)
		(find-symbol "X" p)
	      (unexport sym1 p)
	      (multiple-value-bind (sym2 access2)
		  (find-symbol "X" p)
		(and (eq r t)
		     (eq sym1 sym2)
		     (eq access1 :external)
		     (eq access2 :internal)
		     (equal (symbol-name sym1) "X"))))))
      (error (c) c))
  t)
	
(deftest unexport-2
    (handler-case
	(progn
	  (ignore-errors (delete-package "X"))
	  (let* ((p (make-package "X" :use nil))
		 (r (export (intern "X" p) p)))
	    (multiple-value-bind (sym1 access1)
		(find-symbol "X" p)
	      (unexport (list sym1) "X")
	      (multiple-value-bind (sym2 access2)
		  (find-symbol "X" p)
		(and (eq sym1 sym2)
		     (eq r t)
		     (eq access1 :external)
		     (eq access2 :internal)
		     (equal (symbol-name sym1) "X"))))))
      (error (c) c))
  t)

(deftest unexport-3
    (handler-case
	(progn
	  (ignore-errors (delete-package "X"))
	  (let* ((p (make-package "X" :use nil))
		 (r1 (export (intern "X" p) p))
		 (r2 (export (intern "Y" p) p)))
	    (multiple-value-bind (sym1 access1)
		(find-symbol "X" p)
	      (multiple-value-bind (sym1a access1a)
		  (find-symbol "Y" p)
		(unexport (list sym1 sym1a) '#:|X|)
		(multiple-value-bind (sym2 access2)
		    (find-symbol "X" p)
		  (multiple-value-bind (sym2a access2a)
		      (find-symbol "Y" p)
		      (and (eq sym1 sym2)
			   (eq sym1a sym2a)
			   (eq r1 t)
			   (eq r2 t)
			   (eq access1 :external)
			   (eq access2 :internal)
			   (eq access1a :external)
			   (eq access2a :internal)
			   (equal (symbol-name sym1) "X")
			   (equal (symbol-name sym1a) "Y"))))))))
      (error (c) c))
  t)

(deftest unexport-4
    (handler-case
	(progn
	  (ignore-errors (delete-package "X"))
	  (let* ((p (make-package "X" :use nil))
		 (r (export (intern "X" p) p)))
	    (multiple-value-bind (sym1 access1)
		(find-symbol "X" p)
	      (unexport (list sym1) #\X)
	      (multiple-value-bind (sym2 access2)
		  (find-symbol "X" p)
		(and (eq sym1 sym2)
		     (eq r t)
		     (eq access1 :external)
		     (eq access2 :internal)
		     (equal (symbol-name sym1) "X"))))))
      (error (c) c))
  t)

;; Check that it signals a package error when unexporting
;;  an inaccessible symbol

(deftest unexport-5
    (handler-case
	(progn
	  (when (find-package "X") (delete-package "X"))
	  (unexport 'a (make-package "X" :use nil))
	  nil)
      (package-error () t)
      (error (c) c))
  t)

;; Check that internal symbols are left alone

(deftest unexport-6
    (handler-case
	(progn
	  (when (find-package "X") (delete-package "X"))
	  (let ((p (make-package "X" :use nil)))
	    (let* ((sym (intern "FOO" p))
		   (r (unexport sym p)))
	      (multiple-value-bind (sym2 access)
		  (find-symbol "FOO" p)
		(and (eq r t)
		     (eq access :internal)
		     (eq sym sym2)
		     (equal (symbol-name sym) "FOO"))))))
      (error (c) c))
  t)
