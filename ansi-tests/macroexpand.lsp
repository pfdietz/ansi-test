;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 28 13:43:00 2005
;;;; Contains: Tests of MACROEXPAND

(in-package :cl-test)

(deftest macroexpand.error.1
  (signals-error (macroexpand) program-error)
  t)

(deftest macroexpand.error.2
  (signals-error (macroexpand 'x nil nil) program-error)
  t)

;;; Non-error tests

(deftest macroexpand.1
  (loop for x in *universe*
	unless (or (symbolp x)
		   (consp x)
		   (eql (macroexpand x) x))
	collect (list x (macroexpand x)))
  nil)

(deftest macroexpand.2
  (loop for x in *universe*
	unless (or (symbolp x)
		   (consp x)
		   (eql (macroexpand x nil) x))
	collect (list x (macroexpand x nil)))
  nil)

(deftest macroexpand.3
  (macrolet ((%m (&environment env)
		 `(quote
		   ,(loop for x in *universe*
			  unless (or (symbolp x)
				     (consp x)
				     (eql (macroexpand x env) x))
			  collect (list x (macroexpand x env))))))
    (%m))
  nil)

(deftest macroexpand.4
  (macrolet ((%m () ''foo))
    (macrolet ((%m2 (&environment env)
		    (macroexpand '(%m) env)))
      (%m2)))
  foo)






  
