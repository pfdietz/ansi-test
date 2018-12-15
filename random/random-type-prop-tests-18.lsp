;-*- Mode:     Lisp -*-

(in-package :cl-test)

(def-type-prop-test substitute.1
  'substitute
  (list t t '(vector t))
  3)

(def-type-prop-test substitute.2
  'substitute
  (list 'base-char 'base-char 'base-string)
  3)

(def-type-prop-test substitute.3
  'substitute
  (list 'character 'character '(vector character))
  3)

(def-type-prop-test substitute.4
  'substitute
  (list t t '(vector t) '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.5
  'substitute
  (list 'base-char 'base-char 'base-string '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.6
  'substitute
  (list 'bit 'bit 'bit-vector)
  3)

(def-type-prop-test substitute.7
  'substitute
  (list 'bit 'bit 'bit-vector '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.8
  'substitute
  (list '(integer 0 3) '(integer 0 3) '(vector (integer 0 3)))
  3)

(def-type-prop-test substitute.9
  'substitute
  (list '(integer 0 3) '(integer 0 3) '(vector (integer 0 3))
        '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.10
  'substitute
  (list '(integer 0 7) '(integer 0 7) '(vector (integer 0 7)))
  3)

(def-type-prop-test substitute.11
  'substitute
  (list '(integer 0 15) '(integer 0 15) '(vector (integer 0 15)))
  3)

(def-type-prop-test substitute.12
  'substitute
  (list '(integer 0 15) '(integer 0 15) '(vector (integer 0 15))
        '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.13
  'substitute
  (list '(unsigned-byte 8) '(unsigned-byte 8) '(vector (unsigned-byte 8)))
  3)

(def-type-prop-test substitute.14
  'substitute
  (list '(unsigned-byte 8) '(unsigned-byte 8) '(vector (unsigned-byte 8))
        '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.15
  'substitute
  (list '(signed-byte 8) '(signed-byte 8) '(vector (signed-byte 8)))
  3)

(def-type-prop-test substitute.16
  'substitute
  (list '(signed-byte 8) '(signed-byte 8) '(vector (signed-byte 8))
        '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.17
  'substitute
  (list '(unsigned-byte 7) '(unsigned-byte 7) '(vector (unsigned-byte 7)))
  3)

(def-type-prop-test substitute.18
  'substitute
  (list '(unsigned-byte 7) '(unsigned-byte 7) '(vector (unsigned-byte 7))
        '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.19
  'substitute
  (list '(unsigned-byte 16) '(unsigned-byte 16) '(vector (unsigned-byte 16)))
  3)

(def-type-prop-test substitute.20
  'substitute
  (list '(unsigned-byte 16) '(unsigned-byte 16) '(vector (unsigned-byte 16))
        '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.21
  'substitute
  (list '(signed-byte 16) '(signed-byte 16) '(vector (signed-byte 16)))
  3)

(def-type-prop-test substitute.22
  'substitute
  (list '(signed-byte 16) '(signed-byte 16) '(vector (signed-byte 16))
        '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.23
  'substitute
  (list '(unsigned-byte 15) '(unsigned-byte 15) '(vector (unsigned-byte 15)))
  3)

(def-type-prop-test substitute.24
  'substitute
  (list '(unsigned-byte 15) '(unsigned-byte 15) '(vector (unsigned-byte 15))
        '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.25
  'substitute
  (list '(unsigned-byte 32) '(unsigned-byte 32) '(vector (unsigned-byte 32)))
  3)

(def-type-prop-test substitute.26
  'substitute
  (list '(unsigned-byte 32) '(unsigned-byte 32) '(vector (unsigned-byte 32))
        '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.27
  'substitute
  (list '(signed-byte 32) '(signed-byte 32) '(vector (signed-byte 32)))
  3)

(def-type-prop-test substitute.28
  'substitute
  (list '(signed-byte 32) '(signed-byte 32) '(vector (signed-byte 32))
        '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.29
  'substitute
  (list '(unsigned-byte 31) '(unsigned-byte 31) '(vector (unsigned-byte 31)))
  3)

(def-type-prop-test substitute.30
  'substitute
  (list '(unsigned-byte 31) '(unsigned-byte 31) '(vector (unsigned-byte 31))
        '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.31
  'substitute
  (list t t 'list)
  3)

(def-type-prop-test substitute.32
  'substitute
  (list t t '(cons t (cons t (cons t (cons t (cons t (cons t null)))))))
  3)

(def-type-prop-test substitute.33
  'substitute
  (list '(unsigned-byte 3) '(unsigned-byte 3) '(cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null)))))))
  3)

(def-type-prop-test substitute.34
  'substitute
  (list '(unsigned-byte 3) '(unsigned-byte 3) '(cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))))))
        '(eql :count) '(integer 0 4))
  5)

(def-type-prop-test substitute.35
  'substitute
  (list '(unsigned-byte 3) '(unsigned-byte 3) '(cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))))))
        '(eql :key) `(member 1+ 1- ,#'1+ ,#'1-))
  5)

(def-type-prop-test substitute.36
  'substitute
  (list '(unsigned-byte 3) '(unsigned-byte 3) '(cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))))))
        '(member :test :test-not)
        `(member eq eql = equal equalp
                 ,#'eq ,#'eql ,#'= ,#'equal ,#'equalp))
  5)

(def-type-prop-test substitute.37
  'substitute
  (list '(unsigned-byte 3) '(unsigned-byte 3) '(cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))))))
        '(eql :key) `(member 1+ 1- ,#'1+ ,#'1- nil)
        '(member :test :test-not)
        `(member eq eql = equal equalp
                 ,#'eq ,#'eql ,#'= ,#'equal ,#'equalp))
  7)

(def-type-prop-test substitute.38
  'substitute
  (list '(unsigned-byte 3) '(unsigned-byte 3) '(cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))))))
        '(eql :key) `(member 1+ 1- ,#'1+ ,#'1-)
        '(member :test :test-not)
        `(member eq eql = equal equalp
                 ,#'eq ,#'eql ,#'= ,#'equal ,#'equalp)
        `(eql :key) `(eql ,#'(lambda (x) (error "Bad: ~A" x))))
  9)


(def-type-prop-test substitute.39
  'substitute
  (list '(unsigned-byte 3) '(unsigned-byte 3) '(cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))))))
        '(member :test :test-not)
        `(member eq eql = equal equalp
                 ,#'eq ,#'eql ,#'= ,#'equal ,#'equalp)
        '(eql :count) '(integer 0 4))
  7)

(def-type-prop-test substitute.40
  'substitute
  (list '(unsigned-byte 3) '(unsigned-byte 3) '(cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) (cons (unsigned-byte 3) null))))))
        '(member :test :test-not)
        `(member eq eql = equal equalp
                 ,#'eq ,#'eql ,#'= ,#'equal ,#'equalp)
        '(eql :key) `(member 1+ 1- ,#'1+ ,#'1-)
        '(eql :count) '(integer 0 4))
  7)

(def-type-prop-test substitute.41
  'substitute
  (list 'integer 'integer
        (lambda (x y) `(vector (integer ,(min x y) ,(max x y)))))
  3)

(def-type-prop-test substitute.42
  'substitute
  (list '(eql 0) '(eql 1)
        #'make-random-sequence-type-containing
        '(eql :from-end) '(member t nil))
  5)

(def-type-prop-test substitute.43
  'substitute
  (list '(eql 0) '(eql 1)
        #'make-random-sequence-type-containing
        '(eql :from-end) '(member t nil)
        '(eql :count) '(integer 0 4))
  7)

(def-type-prop-test substitute.44
  'substitute
  (list '(eql 0) '(eql 1)
        #'make-random-sequence-type-containing
        '(eql :start) (lambda (e1 e2 s k) (declare (ignore e1 e2 k))
                              `(integer 0 ,(length s))))
  5)

(def-type-prop-test substitute.45
  'substitute
  (list '(eql 0) '(eql 1)
        #'make-random-sequence-type-containing
        '(eql :end) (lambda (e1 e2 s k) (declare (ignore e1 e2 k))
                              `(integer 0 ,(length s))))
  5)

(def-type-prop-test substitute.46
  'substitute
  (list '(eql 0) '(eql 1)
        #'make-random-sequence-type-containing
        '(eql :start) (lambda (e1 e2 s k) (declare (ignore e1 e2 k))
                              `(integer 0 ,(length s)))
        '(eql :end) (lambda (e1 e2 s k1 p1 k2) (declare (ignore e1 e2 k1 k2))
                            `(integer ,p1 ,(length s))))
  7)

(def-type-prop-test substitute.47
  'substitute
  (list '(eql 0) '(eql 1)
        #'make-random-sequence-type-containing
        '(eql :start) (lambda (e1 e2 s k) (declare (ignore e1 e2 k))
                              `(integer 0 ,(length s)))
        '(eql :from-end) '(member t nil))
  7)

(def-type-prop-test substitute.48
  'substitute
  (list '(eql 0) '(eql 1)
        #'make-random-sequence-type-containing
        '(eql :end) (lambda (e1 e2 s k) (declare (ignore e1 e2 k))
                            `(integer 0 ,(length s)))
        '(eql :from-end) '(member t nil))
  7)

(def-type-prop-test substitute.49
  'substitute
  (list '(eql 0) '(eql 1)
        #'make-random-sequence-type-containing
        '(eql :start) (lambda (e1 e2 s k) (declare (ignore e1 e2 k))
                              `(integer 0 ,(length s)))
        '(eql :end) (lambda (e1 e2 s k1 p1 k2) (declare (ignore e1 e2 k1 k2))
                            `(integer ,p1 ,(length s)))
        '(eql :from-end) '(member t nil))
  9)

(def-type-prop-test substitute.50
  'substitute
  (list '(eql #\a) '(eql #\b)
        #'make-random-sequence-type-containing
        '(eql :start) (lambda (e1 e2 s k) (declare (ignore e1 e2 k))
                              `(integer 0 ,(length s)))
        '(eql :end) (lambda (e1 e2 s k1 p1 k2) (declare (ignore e1 e2 k1 k2))
                            `(integer ,p1 ,(length s)))
        '(eql :from-end) '(member t nil))
  9)

(def-type-prop-test substitute.51
  'substitute
  (list t t
        #'make-random-sequence-type-containing
        '(eql :start) (lambda (e1 e2 s k) (declare (ignore e1 e2 k))
                              `(integer 0 ,(length s)))
        '(eql :end) (lambda (e1 e2 s k1 p1 k2) (declare (ignore e1 e2 k1 k2))
                            `(integer ,p1 ,(length s)))
        '(eql :from-end) '(member t nil))
  9)

(def-type-prop-test substitute.52
  'substitute
  (list t t
        #'make-random-sequence-type-containing
        '(eql :start) (lambda (e1 e2 s k) (declare (ignore e1 e2 k))
                              `(integer 0 ,(length s)))
        '(eql :end) (lambda (e1 e2 s k1 p1 k2) (declare (ignore e1 e2 k1 k2))
                            `(integer ,p1 ,(length s)))
        '(eql :from-end) '(member t nil)
        '(eql :count) '(integer 0 4))
  9)




