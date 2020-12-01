;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar  6 20:39:10 2005
;;;; Contains:  Tests that invoke the random type prop infrastructure, part 3

(in-package :cl-test)

;;; trig, hyperbolic functions here

;;; WARNING -- these tests may cause floating point overflow/underflow
;;; Ignore those failures
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

;;; WARNING -- these tests may cause floating point overflow/underflow
;;; Ignore those failures
(def-type-prop-test /.1 '/ '((and integer (not (satisfies zerop)))) 1)
(def-type-prop-test /.2 '/ '((and rational (not (satisfies zerop)))) 1)
(def-type-prop-test /.3 '/ '((and real (not (satisfies zerop)))) 1 :ignore 'arithmetic-error)
(def-type-prop-test /.4 '/ '((and complex (not (satisfies zerop)))) 1 :ignore 'arithmetic-error)
(def-type-prop-test /.5 '/ '(integer) 2 :maxargs 6 :rest-type '(and integer (not (satisfies zerop))))
(def-type-prop-test /.6 '/ '(rational) 2 :maxargs 6 :rest-type '(and rational (not (satisfies zerop))))
#|
(def-type-prop-test /.7 '/ '(real) 2 :maxargs 6 :rest-type '(and real (not (satisfies zerop)))
 :test #'approx= :ignore 'arithmetic-error)
(def-type-prop-test /.8 '/ '(number) 2 :maxargs 6 :rest-type '(and number (not (satisfies zerop)))
  :test #'(lambda (x y) (approx= x y (* 100 (epsilon x)))) :ignore 'arithmetic-error)
|#

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

(def-type-prop-test exp.1 'exp '((integer -100 100)) 1)
(def-type-prop-test exp.2 'exp '((real -100 100)) 1)
(def-type-prop-test exp.3 'exp '((complex (real -100 100))) 1)

(def-type-prop-test expt.1 'expt '((integer 1 1000) (integer -1000 1000)) 2)
(def-type-prop-test expt.2 'expt '((integer -1000 -1) (integer -1000 1000)) 2)
(def-type-prop-test expt.3 'expt '((rational (0) 1000) (integer -1000 1000)) 2)
(def-type-prop-test expt.4 'expt '((real -1000 (0)) (real -100 100)) 2)
(def-type-prop-test expt.5 'expt '((real -1000 (0)) (eql 1/2)) 2)

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
(def-type-prop-test complexp.2 '(lambda (x y) (complexp (complex x y)))
  '(real real) 2)

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

;; Combined integer tests
(def-type-prop-test logand.lognot.1 '(lambda (x y) (logand x (lognot y))) '(integer integer) 2)
(def-type-prop-test logand.lognot.2 '(lambda (x y) (logand (lognot x) y)) '(integer integer) 2)
(def-type-prop-test logand.lognot.3 '(lambda (x y) (logand (lognot x) (lognot y))) '(integer integer) 2)
(def-type-prop-test logand.lognot.4 '(lambda (x y) (lognot (logand x y))) '(integer integer) 2)

(def-type-prop-test logior.lognot.1 '(lambda (x y) (logior x (lognot y))) '(integer integer) 2)
(def-type-prop-test logior.lognot.2 '(lambda (x y) (logior (lognot x) y)) '(integer integer) 2)
(def-type-prop-test logior.lognot.3 '(lambda (x y) (logior (lognot x) (lognot y))) '(integer integer) 2)
(def-type-prop-test logior.lognot.4 '(lambda (x y) (lognot (logior x y))) '(integer integer) 2)

(def-type-prop-test logxor.lognot.1 '(lambda (x y) (logxor x (lognot y))) '(integer integer) 2)
(def-type-prop-test logxor.lognot.2 '(lambda (x y) (logxor (lognot x) y)) '(integer integer) 2)
(def-type-prop-test logxor.lognot.3 '(lambda (x y) (logxor (lognot x) (lognot y))) '(integer integer) 2)
(def-type-prop-test logxor.lognot.4 '(lambda (x y) (lognot (logxor x y))) '(integer integer) 2)

(def-type-prop-test logand.logior.1 '(lambda (x y z) (logand x (logior y z))) '(integer integer integer) 3)
(def-type-prop-test logand.logior.2 '(lambda (x y z) (logand (logior x y) z)) '(integer integer integer) 3)
(def-type-prop-test logand.logior.3 '(lambda (x y z w) (logand (logior x y) (logior z w))) '(integer integer integer integer) 4)

(def-type-prop-test logior.logand.1 '(lambda (x y z) (logior x (logand y z))) '(integer integer integer) 3)
(def-type-prop-test logior.logand.2 '(lambda (x y z) (logior (logand x y) z)) '(integer integer integer) 3)
(def-type-prop-test logior.logand.3 '(lambda (x y z w) (logior (logand x y) (logand z w))) '(integer integer integer integer) 4)

(def-type-prop-test logbitp.1 'logbitp '((integer 0 32) integer) 2)
(def-type-prop-test logbitp.2 'logbitp '((integer 0 100) integer) 2)
; (def-type-prop-test logbitp.3 'logbitp '((integer 0) integer) 2)

(def-type-prop-test logcount 'logcount '(integer) 1)
(def-type-prop-test logtest 'logtest '(integer integer) 2)

(def-type-prop-test decode-float.1 'decode-float '(float) 1)
(def-type-prop-test decode-float.2 '(lambda (x) (nth-value 1 (decode-float x))) '(float) 1)
(def-type-prop-test decode-float.3 '(lambda (x) (nth-value 2 (decode-float x))) '(float) 1)
(def-type-prop-test float-radix 'float-radix '(float) 1)
(def-type-prop-test scale-float 'scale-float '(float (integer -30 30)) 2 :ignore 'arithmetic-error :test #'approx=)
(def-type-prop-test float-sign.1 'float-sign '(float) 1)
(def-type-prop-test float-sign.2 'float-sign '(float float) 2)
(def-type-prop-test float-digits 'float-digits '(float) 1)
(def-type-prop-test float-precision 'float-precision '(float) 1)
(def-type-prop-test integer-decode-float.1 'integer-decode-float '(float) 1)
(def-type-prop-test integer-decode-float.2 '(lambda (x) (nth-value 1 (integer-decode-float x))) '(float) 1)
(def-type-prop-test integer-decode-float.3 '(lambda (x) (nth-value 2 (integer-decode-float x))) '(float) 1)


(def-type-prop-test float.1 'float '(real) 1)
(def-type-prop-test float.2 'float '(real float) 2)
(def-type-prop-test floatp 'floatp '(t) 1)

(defun has-nonzero-length (x) (> (length x) 0))

(def-type-prop-test parse-integer.1 'parse-integer
  '((and (vector (member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
         (satisfies has-nonzero-length)))
  1)

(def-type-prop-test parse-integer.2 'parse-integer
  `((and (vector (member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
         (satisfies has-nonzero-length))
    (eql :start)
    ,#'(lambda (x &rest rest) (declare (ignore rest))
         `(integer 0 (,(length x)))))
  3)

(def-type-prop-test parse-integer.3 'parse-integer
  `((and (vector (member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
         (satisfies has-nonzero-length))
    (eql :end)
    ,#'(lambda (x &rest rest) (declare (ignore rest))
         `(integer 1 ,(length x))))
  3)

(def-type-prop-test parse-integer.4 'parse-integer
  `((and (vector (member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
         (satisfies has-nonzero-length))
    (eql :junk-allowed)
    (member nil t))
  3)

(def-type-prop-test parse-integer.5 'parse-integer
  `(string (eql :junk-allowed) (and t (not null)))
  3)

(def-type-prop-test parse-integer.6 'parse-integer
  `((and (vector (member #\0 #\1))
         (satisfies has-nonzero-length))
    (eql :radix)
    (integer 2 36))
  3)

(def-type-prop-test parse-integer.7 'parse-integer
  `((and (vector (member #\0 #\1 #\2 #\3))
         (satisfies has-nonzero-length))
    (eql :radix)
    (integer 4 36))
  3)

(def-type-prop-test parse-integer.8 'parse-integer
  `((and (vector (member #\0 #\1 #\2 #\3 #\4 #\5))
         (satisfies has-nonzero-length))
    (eql :radix)
    (integer 6 36))
  3)

(def-type-prop-test parse-integer.9 'parse-integer
  `((and (vector (member ,@(map 'list #'identity
                                "0123456789abcdefghijklmnopqrstuvwxyz")))
         (satisfies has-nonzero-length))
    (eql :radix)
    (integer 36 36))
  3)

(def-type-prop-test sxhash 'sxhash '(t) 1)
(def-type-prop-test sxhash.2 'sxhash '(string) 1)
(def-type-prop-test sxhash.3 '(lambda (s) (unless (= (sxhash s) (sxhash (map 'string #'identity s))) (error "Bad sxhash: ~a" s))) '(string) 1)

(def-type-prop-test boole.1 'boole `(,(list 'member boole-1 boole-2 boole-andc1
                                      boole-andc2 boole-and boole-c1 boole-c2
                                      boole-clr boole-eqv boole-ior
                                      boole-nand boole-nor boole-orc1
                                      boole-orc2 boole-set boole-xor)
                                     integer integer) 3)

;; (def-type-prop-test byte.1 'byte '((integer 0) (integer 0)) 2)
;; (def-type-prop-test byte.2 'byte '((integer 0 10) (integer 0)) 2)

(def-type-prop-test byte.3 'byte '((integer 0 1000) (integer 0 1000)) 2)
(def-type-prop-test deposit-field.1
  '(lambda (m s p n)
    (deposit-field m (byte s p) n))
  '(integer (integer 0 100) (integer 0 100) integer)
  4)
(def-type-prop-test dpb.1
  '(lambda (m s p n)
    (dpb m (byte s p) n))
  '(integer (integer 0 100) (integer 0 100) integer)
  4)
(def-type-prop-test ldb.1
  '(lambda (s p n)
    (ldb (byte s p) n))
  '((integer 0 100) (integer 0 100) integer)
  3)
(def-type-prop-test ldb.2
  '(lambda (s p n x)
    (values (setf (ldb (byte s p) n) x)
     n))
  '((integer 0 100) (integer 0 100) integer integer)
  4)
(def-type-prop-test ldb-test.1
  '(lambda (s p n) (ldb-test (byte s p) n))
  '((integer 0 100) (integer 0 100) integer)
  3)
(def-type-prop-test mask-field.1
  '(lambda (s p n)
    (mask-field (byte s p) n))
  '((integer 0 100) (integer 0 100) integer)
  3)
(def-type-prop-test mask-field.2
  '(lambda (s p n x)
    (values (setf (mask-field (byte s p) n) x)
     n))
  '((integer 0 100) (integer 0 100) integer integer)
  4)
