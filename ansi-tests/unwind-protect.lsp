;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 14:41:16 2002
;;;; Contains: Tests of UNWIND-PROTECT

(in-package :cl-test)

(deftest unwind-protect.1
  (let ((x nil))
    (unwind-protect
	(push 1 x)
      (incf (car x))))
  (2))

(deftest unwind-protect.2
  (let ((x nil))
    (block foo
      (unwind-protect
	  (progn (push 1 x) (return-from foo x))
	(incf (car x)))))
  (2))

(deftest unwind-protect.3
  (let ((x nil))
    (tagbody
      (unwind-protect
	  (progn (push 1 x) (go done))
	(incf (car x)))
      done)
    x)
  (2))

(deftest unwind-protect.4
  (let ((x nil))
    (catch 'done
      (unwind-protect
	  (progn (push 1 x) (throw 'done x))
	(incf (car x)))))
  (2))

(deftest unwind-protect.5
  (let ((x nil))
    (ignore-errors
      (unwind-protect
	  (progn (push 1 x) (error "Boo!"))
	(incf (car x))))
    x)
  (2))

(deftest unwind-protect.6
  (let ((x nil))
    (block done
      (flet ((%f () (return-from done nil)))
	(unwind-protect (%f)
	  (push 'a x))))
    x)
  (a))

(deftest unwind-protect.7
  (let ((x nil))
    (block done
      (flet ((%f () (return-from done nil)))
	(unwind-protect
	    (unwind-protect (%f)
	      (push 'b x))
	  (push 'a x))))
    x)
  (a b))

(deftest unwind-protect.8
  (let ((x nil))
    (block done
      (unwind-protect
	  (flet ((%f () (return-from done nil)))
	    (unwind-protect
		(unwind-protect (%f)
		  (push 'b x))
	      (push 'a x)))
	(push 'c x)))
    x)
  (c a b))

(deftest unwind-protect.9
  (let ((x nil))
    (handler-case
      (flet ((%f () (error 'type-error :datum 'foo :expected-type nil)))
	(unwind-protect (handler-case (%f))
	  (push 'a x)))
      (type-error () x)))
  (a))
