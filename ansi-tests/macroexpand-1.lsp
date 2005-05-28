;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 28 13:47:32 2005
;;;; Contains: Tests of MACROEXPAND-1

(in-package :cl-test)

(deftest macroexpand-1.error.1
  (signals-error (macroexpand-1) program-error)
  t)

(deftest macroexpand-1.error.2
  (signals-error (macroexpand-1 'x nil nil) program-error)
  t)

;;; Non-error tests

(deftest macroexpand-1.1
  (loop for x in *universe*
	unless (or (symbolp x)
		   (consp x)
		   (eql (macroexpand-1 x) x))
	collect (list x (macroexpand-1 x)))
  nil)

(deftest macroexpand-1.2
  (loop for x in *universe*
	unless (or (symbolp x)
		   (consp x)
		   (eql (macroexpand-1 x nil) x))
	collect (list x (macroexpand-1 x nil)))
  nil)

(deftest macroexpand-1.3
  (macrolet ((%m (&environment env)
		 `(quote
		   ,(loop for x in *universe*
			  unless (or (symbolp x)
				     (consp x)
				     (eql (macroexpand-1 x env) x))
			  collect (list x (macroexpand-1 x env))))))
    (%m))
  nil)

(deftest macroexpand-1.4
  (macrolet ((%m () ''foo))
    (macrolet ((%m2 (&environment env)
		    (macroexpand-1 '(%m) env)))
      (%m2)))
  foo)






  

