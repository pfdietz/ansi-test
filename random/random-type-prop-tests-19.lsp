;-*- Mode:     Lisp -*-

(in-package :cl-test)

(def-type-prop-test sin.1 'sin '(real) 1)
(def-type-prop-test sin.2 'sin '(single-float) 1)
(def-type-prop-test sin.3 'sin '(double-float) 1)
(def-type-prop-test sin.4 'sin '(long-float) 1)
(def-type-prop-test sin.5 'sin '(short-float) 1)
(def-type-prop-test cos.1 'cos '(real) 1)
(def-type-prop-test cos.2 'cos '(single-float) 1)
(def-type-prop-test cos.3 'cos '(double-float) 1)
(def-type-prop-test cos.4 'cos '(long-float) 1)
(def-type-prop-test cos.5 'cos '(short-float) 1)
(def-type-prop-test tan.1 'tan '((real (#.(- (/ pi 2))) (#.(/ pi 2)))) 1)

;;; Apply idioms

(def-type-prop-test apply.1 '(lambda (x) (apply #'list x)) '(list) 1)
(def-type-prop-test apply.2 '(lambda (x y) (apply #'list x y)) '(t list) 2)
(def-type-prop-test apply.3 '(lambda (x) (apply #'vector x)) '(list) 1)
(def-type-prop-test apply.4 '(lambda (x) (apply #'values x)) '(list) 1)

;;; make-array

(def-type-prop-test make-array.1
  'make-array (list '(or (integer 0 10)
                      (cons (integer 0 10) null))
                    '(eql :initial-element)
                    'integer)
  3
  :test #'array-equal)

(def-type-prop-test make-array.2 'make-array (list '(integer 0 10)
                                                   '(eql :initial-contents)
                                                   #'(lambda (i x)
                                                       (declare (ignore x))
                                                       (make-list-type i 'null 'integer)))
  3
  :test #'array-equal)

(def-type-prop-test make-array.3 'make-array (list '(integer 0 10)
                                                   '(eql :initial-contents)
                                                   #'(lambda (i x)
                                                       (declare (ignore x))
                                                       `(simple-array * (,i))))
  3
  :test #'array-equal)

(def-type-prop-test make-array.4 'make-array (list '(integer 0 10)
                                                   '(eql :element-type)
                                                   `(member ,@(loop for i from 1 to 64
                                                                    collect `(integer 0 ,(1- (ash 1 i)))))
                                                   '(eql :initial-contents)
                                                   #'(lambda (i k1 etp k2)
                                                       (declare (ignore k1 k2))
                                                       `(simple-array ,etp (,i))))
  5
  :test #'array-equal)

(def-type-prop-test make-array.5 'make-array (list '(integer 0 10)
                                                   '(eql :element-type)
                                                   '(member base-char standard-char character)
                                                   '(eql :initial-element)
                                                   #'(lambda (i k1 etp k2)
                                                       (declare (ignore i k1 k2))
                                                       etp))
  5
  :test #'array-equal)

(def-type-prop-test make-array.6 'make-array (list '(integer 0 10)
                                                   '(eql :element-type)
                                                   '(member base-char standard-char character)
                                                   '(eql :initial-contents)
                                                   #'(lambda (i k1 etp k2)
                                                       (declare (ignore k1 k2))
                                                       (make-simple-sequence-type i etp)))
  5
  :test #'array-equal)



