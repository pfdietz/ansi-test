;-*- Mode:     Lisp -*-

(in-package :cl-test)

(defmacro if-typep (e x1 x2 x3)
  `(typep (if ,e ,x1 ,x2) (type-of ,x3)))

(def-type-prop-test if-typep.1 'if-typep `(t t t t) 4)

(def-type-prop-test typep-typeof.1
  '(lambda (x y) (typep x (type-of y)))
  '(t t)
  2)

(def-type-prop-test typep-class-of.1
  '(lambda (x y) (typep x (class-of y)))
  '(t t)
  2)

(def-type-prop-test +.*.1
  '(lambda (x y z) (+ x (* y z)))
  '(integer integer integer)
  3)

(def-type-prop-test +.*.2
  '(lambda (x y z) (+ x (* y z)))
  '(rational rational rational)
  3)

(def-type-prop-test case.1
  '(lambda (c x y z)
    (case c (1 x) (2 y) (t z)))
  '((or (member 1 2) t)
    t t t)
  4)

(def-type-prop-test if.+.1
  '(lambda (b x y z)
    (+ x (if b y z)))
  '(t integer integer integer)
  4)

(def-type-prop-test if.+.2
  '(lambda (b x y z)
    (+ (if b y z) x))
  '(t integer integer integer)
  4)

(def-type-prop-test if.+.3
  '(lambda (b x y z)
    (+ x (if b y z)))
  '(t real real real)
  4)

(def-type-prop-test if.+.4
  '(lambda (b x y z)
    (+ x (if b y z)))
  '(t real real real)
  4)

(def-type-prop-test concatenate.1
  'concatenate
  '((eql list) sequence sequence)
  3)

(def-type-prop-test concatenate.2
  'concatenate
  (list '(member string simple-string)
        #'(lambda (&rest x) (declare (ignore x)) (make-sequence-type (random 10) 'character))
        #'(lambda (&rest x) (declare (ignore x)) (make-sequence-type (random 10) 'character)))
  3)

(def-type-prop-test concatenate.3
  'concatenate
  (list '(member base-string simple-base-string)
        #'(lambda (&rest x) (declare (ignore x)) (make-sequence-type (random 10) 'base-char))
        #'(lambda (&rest x) (declare (ignore x)) (make-sequence-type (random 10) 'base-char)))
  3)

(def-type-prop-test concatenate.4
  'concatenate
  '((eql vector) sequence sequence)
  3)

(def-type-prop-test concatenate.5
  'concatenate
  (list '(member bit-vector simple-bit-vector)
        #'(lambda (&rest x) (declare (ignore x)) (make-sequence-type (random 10) 'bit))
        #'(lambda (&rest x) (declare (ignore x)) (make-sequence-type (random 10) 'bit)))
  3)

(def-type-prop-test remove.1
  'remove
  (list t #'make-random-sequence-type-containing)
  2)

(def-type-prop-test remove.2
  'remove
  (list t #'make-random-sequence-type-containing '(eql :from-end) '(member nil t))
  4)

(def-type-prop-test remove.3
  'remove
  (list 'bit 'bit-vector)
  2)

(def-type-prop-test remove.4
  'remove
  (list 'bit 'simple-bit-vector)
  2)

(def-type-prop-test remove.5
  'remove
  (list 'character 'string)
  2)

(def-type-prop-test remove.6
  'remove
  (list 'character 'simple-string)
  2)

(def-type-prop-test remove.7
  'remove
  (list t #'make-random-sequence-type-containing
        '(eql :start) #'start-type-for-v2)
  4)

(def-type-prop-test remove.8
  'remove
  (list t #'make-random-sequence-type-containing
        '(eql :end) #'start-type-for-v2)
  4)

(def-type-prop-test remove.9
  'remove
  (list t #'make-random-sequence-type-containing
        '(eql :start) #'start-type-for-v2
        '(eql :end) #'(lambda (v1 v2 sk si ek) (declare (ignore v1 sk ek))
                              `(integer ,si ,(max si (length v2)))))
  6)

(def-type-prop-test remove.10
  'remove
  (list t #'make-random-sequence-type-containing
        '(eql :count) '(integer 0 3))
  4)

(def-type-prop-test remove.10a
  'remove
  (list t #'make-random-sequence-type-containing
        '(eql :count) 'integer)
  4)

(def-type-prop-test remove.11
  'remove
  (list t #'make-random-sequence-type-containing
        '(eql :start) #'start-type-for-v2
        '(eql :count) '(integer 0 3))
  6)

(def-type-prop-test remove.12
  'remove
  (list t #'make-random-sequence-type-containing
        '(eql :start) #'start-type-for-v2
        '(eql :end) #'(lambda (v1 v2 sk si ek) (declare (ignore v1 sk ek))
                              `(integer ,si ,(length v2)))
        '(eql :count) '(integer 0 3))
  8)

(def-type-prop-test remove.13
  'remove
  (list t #'make-random-sequence-type-containing
        '(eql :count) '(integer 0 3)
        '(eql :from-end) '(eql t))
  6)

(def-type-prop-test remove.14
  'remove
  (list t #'make-random-sequence-type-containing
        '(eql :start) #'start-type-for-v2
        '(eql :count) '(integer 0 3)
        '(eql :from-end) '(eql t))
  8)

(def-type-prop-test remove.15
  'remove
  (list t #'make-random-sequence-type-containing
        '(eql :start) #'start-type-for-v2
        '(eql :end) #'(lambda (v1 v2 sk si ek) (declare (ignore v1 sk ek))
                              `(integer ,si ,(max si (length v2))))
        '(eql :count) '(integer 0 3)
        '(eql :from-end) '(eql t))
  10)
