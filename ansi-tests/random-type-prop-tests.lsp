;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Feb 20 11:50:26 2005
;;;; Contains: Randomized tests of type propagation during compilation

(compile-and-load "random-type-prop.lsp")

(in-package :cl-test)

(defmacro def-type-prop-test (name &body args)
  `(deftest ,(intern (concatenate 'string "RANDOM-TYPE-PROP."
				  (string name))
		     (find-package :cl-test))
     (do-random-type-prop-tests ,@args)
     nil))

(def-type-prop-test special-operator-p 'special-operator-p '(symbol) 1)
(def-type-prop-test type-of 'type-of '(t) 1)
(def-type-prop-test typep.1 '(lambda (x y) (typep x (type-of y))) '(t t) 2)
(def-type-prop-test typep.2 'typep
  (list t #'(lambda (x)
	      (let ((type (make-random-type-containing x)))
		`(eql ,type))))
  2)
(def-type-prop-test subtypep
  '(lambda (x y) (subtypep (type-of x) (type-of y))) '(t t) 2)
(def-type-prop-test fboundp.1 'fboundp '(symbol) 1)
(def-type-prop-test fboundp.2 'fboundp '((cons (eql setf) (cons symbol null))) 1)
(def-type-prop-test functionp 'functionp '(t) 1)
(def-type-prop-test compiled-function-p 'compiled-function-p '(t) 1)
(def-type-prop-test not 'not '(t) 1)
(def-type-prop-test eq 'eq (list
			    '(and t (not number) (not character))
			    #'(lambda (x) (rcase
					   (1 `(eql ,x))
					   (1 '(and t (not number) (not character))))))
  2)
(def-type-prop-test eql.1 'eql '(t t) 2)
(def-type-prop-test eql.2 'eql (list t #'(lambda (x) `(eql ,x))) 2)
(def-type-prop-test equal.1 'equal '(t t) 2)
(def-type-prop-test equal.2 'equal (list t #'(lambda (x) `(eql ,x))) 2)
(def-type-prop-test equalp.1 'equalp '(t t) 2)
(def-type-prop-test equalp.2 'equalp (list t #'(lambda (x) `(eql ,x))) 2)
(def-type-prop-test identity 'identity '(t) 1)
(def-type-prop-test complement
 '(lambda (f y) (funcall (complement f) y)) (list `(eql ,#'symbolp) t) 2)
(def-type-prop-test constantly
  '(lambda (x) (funcall (constantly x))) '(t) 1)
(def-type-prop-test and.1 'and '(t) 1)
(def-type-prop-test and.2 'and '((or null t) t) 2)
(def-type-prop-test and.3 'and '((or null t) (or null t) t) 3)
(def-type-prop-test if.1 'if '(boolean t) 2)
(def-type-prop-test if.2 'if '(boolean t t) 3)
(def-type-prop-test or.1 'or '(t) 1)
(def-type-prop-test or.2 'or '((or null t) t) 2)
(def-type-prop-test or.3 'or '((or null null t) (or null t) t) 3)
(def-type-prop-test when 'when '((or null t) t) 2)
(def-type-prop-test unless 'unless '((or null t) t) 2)
(def-type-prop-test slot-exists-p 'slot-exists-p '(t symbol) 2)
(def-type-prop-test find-class 'find-class '(symbol null) 2)
(def-type-prop-test class-of 'class-of '(t) 1)
(def-type-prop-test find-restart 'find-restart '((and symbol (not null))) 1)
(def-type-prop-test symbolp 'symbolp '(t) 1)
(def-type-prop-test keywordp 'keywordp '(t) 1)
(def-type-prop-test make-symbol 'make-symbol '(string) 1
  :test #'(lambda (x y) (string= (symbol-name x) (symbol-name y))))
(def-type-prop-test symbol-name 'symbol-name '(symbol) 1)
(def-type-prop-test symbol-package 'symbol-package '(symbol) 1)
(def-type-prop-test boundp 'boundp '(symbol) 1)
(def-type-prop-test find-symbol 'find-symbol '(string) 1)
(def-type-prop-test find-package 'find-package '((or string symbol character)) 1)

(def-type-prop-test =.1 '= '(number number) 2)
(def-type-prop-test =.2 '= '(number number number) 3)
(def-type-prop-test =.3 '= nil 4 :maxargs 10 :rest-type 'number)
(def-type-prop-test =.4 '= '(integer integer) 2)
(def-type-prop-test =.5 '= (list 'number #'(lambda (x) (if (coin) 'number
							`(eql ,x)))) 2)
(def-type-prop-test =.6 '= (list 'number 'number
				 #'(lambda (x y) (rcase
						  (2 'number)
						  (1 `(eql ,x))
						  (1 `(eql ,y)))))
  3)
(def-type-prop-test /=.1 '/= '(number number) 2)
(def-type-prop-test /=.2 '/= '(number number number) 3)
(def-type-prop-test /=.3 '/= nil 4 :maxargs 10 :rest-type 'number)
(def-type-prop-test /=.4 '/= '(integer integer) 2)
(def-type-prop-test /=.5 '/= (list 'number #'(lambda (x) (if (coin) 'number
							   `(eql ,x)))) 2)
(def-type-prop-test /=.6 '/= (list 'number 'number
				   #'(lambda (x y) (rcase
						    (2 'number)
						    (1 `(eql ,x))
						    (1 `(eql ,y)))))
  3)
(def-type-prop-test <.1 '< '(real real) 2)
(def-type-prop-test <.2 '< '(real real real) 3)
(def-type-prop-test <.3 '< nil 4 :maxargs 10 :rest-type 'real)
(def-type-prop-test <.4 '< '(integer integer) 2)
(def-type-prop-test <.5 '< (list 'real #'(lambda (x) (if (coin) 'real
                                                        `(eql ,x)))) 2)
(def-type-prop-test <.6 '< (list 'real 'real
                                 #'(lambda (x y) (rcase
                                                  (2 'real)
                                                  (1 `(eql ,x))
                                                  (1 `(eql ,y)))))
  3)
(def-type-prop-test >.1 '> '(real real) 2)
(def-type-prop-test >.2 '> '(real real real) 3)
(def-type-prop-test >.3 '> nil 4 :maxargs 10 :rest-type 'real)
(def-type-prop-test >.4 '> '(integer integer) 2)
(def-type-prop-test >.5 '> (list 'real #'(lambda (x) (if (coin) 'real
                                                        `(eql ,x)))) 2)
(def-type-prop-test >.6 '> (list 'real 'real
                                 #'(lambda (x y) (rcase
                                                  (2 'real)
                                                  (1 `(eql ,x))
                                                  (1 `(eql ,y)))))
  3)
(def-type-prop-test <=.1 '<= '(real real) 2)
(def-type-prop-test <=.2 '<= '(real real real) 3)
(def-type-prop-test <=.3 '<= nil 4 :maxargs 10 :rest-type 'real)
(def-type-prop-test <=.4 '<= '(integer integer) 2)
(def-type-prop-test <=.5 '<= (list 'real #'(lambda (x) (if (coin) 'real
							 `(eql ,x)))) 2)
(def-type-prop-test <=.6 '<= (list 'real 'real
				   #'(lambda (x y) (rcase
						    (2 'real)
						    (1 `(eql ,x))
						    (1 `(eql ,y)))))
  3)
(def-type-prop-test >=.1 '>= '(real real) 2)
(def-type-prop-test >=.2 '>= '(real real real) 3)
(def-type-prop-test >=.3 '>= nil 4 :maxargs 10 :rest-type 'real)
(def-type-prop-test >=.4 '>= '(integer integer) 2)
(def-type-prop-test >=.5 '>= (list 'real #'(lambda (x) (if (coin) 'real
							 `(eql ,x)))) 2)
(def-type-prop-test >=.6 '>= (list 'real 'real
				   #'(lambda (x y) (rcase
						    (2 'real)
						    (1 `(eql ,x))
						    (1 `(eql ,y)))))
  3)

(def-type-prop-test min.1 'min nil 2 :maxargs 6 :rest-type 'integer)
(def-type-prop-test min.2 'min nil 2 :maxargs 6 :rest-type 'rational)
(def-type-prop-test min.3 'min nil 2 :maxargs 6 :rest-type 'real)
(def-type-prop-test max.1 'max nil 2 :maxargs 6 :rest-type 'integer)
(def-type-prop-test max.2 'max nil 2 :maxargs 6 :rest-type 'rational)
(def-type-prop-test max.3 'max nil 2 :maxargs 6 :rest-type 'real)

(def-type-prop-test minusp 'minusp '(real) 1)
(def-type-prop-test plusp 'plusp '(real) 1)
(def-type-prop-test zerop 'zerop '(number) 1)

(def-type-prop-test floor.1 'floor '(real) 1)
(def-type-prop-test floor.2 'floor '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test floor.3 'floor '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test ffloor.1 'ffloor '(real) 1)
(def-type-prop-test ffloor.2 'ffloor '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test ffloor.3 'ffloor '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test ceiling.1 'ceiling '(real) 1)
(def-type-prop-test ceiling.2 'ceiling '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test ceiling.3 'ceiling '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test fceiling.1 'fceiling '(real) 1)
(def-type-prop-test fceiling.2 'fceiling '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test fceiling.3 'fceiling '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test truncate.1 'truncate '(real) 1)
(def-type-prop-test truncate.2 'truncate '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test truncate.3 'truncate '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test ftruncate.1 'ftruncate '(real) 1)
(def-type-prop-test ftruncate.2 'ftruncate '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test ftruncate.3 'ftruncate '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test round.1 'round '(real) 1)
(def-type-prop-test round.2 'round '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test round.3 'round '(real (and real (not (satisfies zerop)))) 2)
(def-type-prop-test fround.1 'fround '(real) 1)
(def-type-prop-test fround.2 'fround '(real (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test fround.3 'fround '(real (and real (not (satisfies zerop)))) 2)

;;; trig, hyperbolic functions here

(def-type-prop-test *.1 '* '(integer integer) 2)
(def-type-prop-test *.2 '* nil 1 :rest-type 'integer :maxargs 4)
(def-type-prop-test *.3 '* nil 2 :rest-type 'integer :maxargs 10)
(def-type-prop-test *.4 '* '(real real) 2  :test #'approx=)
(def-type-prop-test *.5 '* '(number number) 2 :test #'approx=)

(def-type-prop-test \+.1 '+ '(integer integer) 2)
(def-type-prop-test \+.2 '+ nil 1 :rest-type 'integer :maxargs 4)
(def-type-prop-test \+.3 '+ nil 2 :rest-type 'integer :maxargs 10)
(def-type-prop-test \+.4 '+ '(real real) 2 :test #'approx=)
(def-type-prop-test \+.5 '+ '(number number) 2 :test #'approx=)

(def-type-prop-test \-.1 '- '(integer integer) 2)
(def-type-prop-test \-.2 '- nil 1 :rest-type 'integer :maxargs 4)
(def-type-prop-test \-.3 '- nil 2 :rest-type 'integer :maxargs 10)
(def-type-prop-test \-.4 '- '(real real) 2 :test #'approx=)
(def-type-prop-test \-.5 '- '(number number) 2 :test #'approx=)
(def-type-prop-test \-.6 '- '(number) 1)

(def-type-prop-test /.1 '/ '((and integer (not (satisfies zerop)))) 1)
(def-type-prop-test /.2 '/ '((and rational (not (satisfies zerop)))) 1)
(def-type-prop-test /.3 '/ '((and real (not (satisfies zerop)))) 1)
(def-type-prop-test /.4 '/ '((and complex (not (satisfies zerop)))) 1)

(def-type-prop-test /.5 '/ '(integer) 2 :maxargs 6 :rest-type '(and integer (not (satisfies zerop))))
(def-type-prop-test /.6 '/ '(rational) 2 :maxargs 6 :rest-type '(and rational (not (satisfies zerop))))
(def-type-prop-test /.7 '/ '(real) 2 :maxargs 6 :rest-type '(and real (not (satisfies zerop)))
 :test #'approx=)
(def-type-prop-test /.8 '/ '(number) 2 :maxargs 6 :rest-type '(and number (not (satisfies zerop)))
  :test #'approx=)

(def-type-prop-test 1+.1 '1+ '(integer) 1)
(def-type-prop-test 1+.2 '1+ '(rational) 1)
(def-type-prop-test 1+.3 '1+ '(real) 1)
(def-type-prop-test 1+.4 '1+ '(number) 1)

(def-type-prop-test 1-.1 '1- '(integer) 1)
(def-type-prop-test 1-.2 '1- '(rational) 1)
(def-type-prop-test 1-.3 '1- '(real) 1)
(def-type-prop-test 1-.4 '1- '(number) 1)

(def-type-prop-test abs.1 'abs '(integer) 1)
(def-type-prop-test abs.2 'abs '(rational) 1)
(def-type-prop-test abs.3 'abs '(real) 1)
(def-type-prop-test abs.4 'abs '(number) 1)

(def-type-prop-test evenp 'evenp '(integer) 1)
(def-type-prop-test oddp 'oddp '(integer) 1)

;;; exp, expt here

(def-type-prop-test gcd 'gcd nil 1 :maxargs 6 :rest-type 'integer)
(def-type-prop-test lcm 'lcm nil 1 :maxargs 6 :rest-type 'integer)

(def-type-prop-test log.1 'log '((and real (not (satisfies zerop)))) 1 :test #'approx=)
(def-type-prop-test log.2 'log '((and number (not (satisfies zerop)))) 1 :test #'approx=)

(def-type-prop-test mod.1 'mod '(integer (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test mod.2 'mod '(real (and real (not (satisfies zerop)))) 2 :test #'approx=)
(def-type-prop-test rem.1 'rem '(integer (and integer (not (satisfies zerop)))) 2)
(def-type-prop-test rem.2 'rem '(real (and real (not (satisfies zerop)))) 2 :test #'approx=)

(def-type-prop-test signum.1 'signum '(integer) 1)
(def-type-prop-test signum.2 'signum '(rational) 1)
(def-type-prop-test signum.3 'signum '(real) 1)
(def-type-prop-test signum.4 'signum '(number) 1)

(def-type-prop-test sqrt.1 'sqrt '(integer) 1 :test #'approx=)
(def-type-prop-test sqrt.2 'sqrt '(rational) 1 :test #'approx=)
(def-type-prop-test sqrt.3 'sqrt '(real) 1 :test #'approx=)
(def-type-prop-test sqrt.4 'sqrt '(number) 1 :test #'approx=)

(def-type-prop-test isqrt 'isqrt '((integer 0)) 1)

(def-type-prop-test numberp 'numberp '(t) 1)

(def-type-prop-test complex.1 'complex '(integer) 1)
(def-type-prop-test complex.2 'complex '(rational) 1)
(def-type-prop-test complex.3 'complex '(real) 1)
(def-type-prop-test complex.4 'complex '(rational rational) 2)
(def-type-prop-test complex.5 'complex '(real real) 2)

(def-type-prop-test complexp 'complexp '(t) 1)

(def-type-prop-test conjugate 'conjugate '(number) 1)

(def-type-prop-test phase.1 'phase '(real) 1)
(def-type-prop-test phase.2 'phase '(number) 1 :test #'approx=)

(def-type-prop-test realpart.1 'realpart '(real) 1)
(def-type-prop-test realpart.2 'realpart '(number) 1)
(def-type-prop-test imagpart.1 'imagpart '(real) 1)
(def-type-prop-test imagpart.2 'imagpart '(number) 1)

(def-type-prop-test realp 'realp '(t) 1)

(def-type-prop-test numerator 'numerator '(rational) 1)
(def-type-prop-test denominator 'denominator '(rational) 1)

(def-type-prop-test rational 'rational '(real) 1)
(def-type-prop-test rationalize 'rationalize '(real) 1)

(def-type-prop-test rationalp 'rationalp '(t) 1)

(def-type-prop-test ash.1 'ash '(integer (integer -32 32)) 2)
(def-type-prop-test ash.2 'ash '(integer (integer -100 100)) 2)

(def-type-prop-test integer-length 'integer-length '(integer) 1)
(def-type-prop-test integerp 'integerp '(t) 1)

(def-type-prop-test logand.1 'logand '(integer integer) 2)
(def-type-prop-test logand.2 'logand nil 2 :rest-type 'integer :maxargs 6)

(def-type-prop-test logandc1 'logandc1 '(integer integer) 2)
(def-type-prop-test logandc2 'logandc2 '(integer integer) 2)

(def-type-prop-test lognand 'lognand '(integer integer) 2)
(def-type-prop-test lognor 'lognor '(integer integer) 2)

(def-type-prop-test logeqv.1 'logeqv '(integer integer) 2)
(def-type-prop-test logeqv.2 'logeqv nil 2 :rest-type 'integer :maxargs 6)

(def-type-prop-test logior.1 'logior '(integer integer) 2)
(def-type-prop-test logior.2 'logior nil 2 :rest-type 'integer :maxargs 6)

(def-type-prop-test logxor.1 'logxor '(integer integer) 2)
(def-type-prop-test logxor.2 'logxor nil 2 :rest-type 'integer :maxargs 6)

(def-type-prop-test logorc1 'logorc1 '(integer integer) 2)
(def-type-prop-test logorc2 'logorc2 '(integer integer) 2)

(def-type-prop-test lognot 'lognot '(integer) 1)

(def-type-prop-test logbitp.1 'logbitp '((integer 0 32) integer) 2)
(def-type-prop-test logbitp.2 'logbitp '((integer 0 100) integer) 2)
; (def-type-prop-test logbitp.3 'logbitp '((integer 0) integer) 2)

(def-type-prop-test logcount 'logcount '(integer) 1)
(def-type-prop-test logtest 'logtest '(integer integer) 2)

(def-type-prop-test float.1 'float '(real) 1)
(def-type-prop-test float.2 'float '(real float) 2)
(def-type-prop-test floatp 'floatp '(t) 1)

