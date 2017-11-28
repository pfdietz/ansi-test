;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Contains: Random type prop tests, part 11 (sequences, cont.)

(in-package :cl-test)

;;; MISMATCH

(def-type-prop-test mismatch.1 'mismatch
  (list 'sequence 'sequence)
  2)

(def-type-prop-test mismatch.2 'mismatch
  (list 'bit-vector 'bit-vector)
  2)

(def-type-prop-test mismatch.3 'mismatch
  (list '(vector * 1) 'sequence)
  2)

(def-type-prop-test mismatch.4 'mismatch
  (list '(vector * 2) 'sequence '(eql :from-end) '(or null t))
  4)

(def-type-prop-test mismatch.5 'mismatch
  (list 'sequence 'sequence '(eql :key)
        (list 'member 'identity nil #'identity 'not #'not))
  4)

(def-type-prop-test mismatch.6 'mismatch
  (list #'(lambda () (make-sequence-type
                      (random 10)
                      (let ((i1 (make-random-integer))
                            (i2 (make-random-integer)))
                        `(integer ,(min i1 i2) ,(max i1 i2)))))
        #'(lambda (s)
            (declare (ignore s))
            (make-sequence-type
             (random 10)
             (let ((i1 (make-random-integer))
                   (i2 (make-random-integer)))
               `(integer ,(min i1 i2) ,(max i1 i2))))))
  2)

(def-type-prop-test mismatch.7 'mismatch
  (list #'(lambda () (make-sequence-type
                      (random 10)
                      (let ((i1 (make-random-integer))
                            (i2 (make-random-integer)))
                        `(integer ,(min i1 i2) ,(max i1 i2)))))
        #'(lambda (s)
            (declare (ignore s))
            (make-sequence-type
             (random 10)
             (let ((i1 (make-random-integer))
                   (i2 (make-random-integer)))
               `(integer ,(min i1 i2) ,(max i1 i2)))))
        '(eql :test)
        (list 'member 'eql #'eql 'equal #'equal '= #'=
              '/= #'/= #'(lambda (x y) (= (logand x 1) (logand y 1)))))
  4)

(def-type-prop-test mismatch.8 'mismatch
  (labels ((%random-char-type () (random-from-seq #(base-char standard-char character)))
           (%random-char-sequence-type (&rest ignored)
              (declare (ignore ignored))
              (make-sequence-type (random 10) (%random-char-type))))

          (list #'%random-char-sequence-type
                #'%random-char-sequence-type
                '(member :test :test-not)
                (let ((char-compare-funs
                       '(char= char/= char< char> char<= char>=
                         char-equal char-not-equal char-lessp char-greaterp
                         char-not-lessp char-not-greaterp)))
                  `(member ,@char-compare-funs
                           ,@(mapcar #'symbol-function char-compare-funs)))))
  4)

(def-type-prop-test mismatch.9 'mismatch
  (list 'sequence 'sequence
        '(eql :start1)
        #'(lambda (s1 s2 k)
            (declare (ignore s2 k))
            (let ((len (length s1)))
              `(integer 0 ,len))))
  4)

(def-type-prop-test mismatch.10 'mismatch
  (list 'sequence 'sequence
        '(eql :end1)
        #'(lambda (s1 s2 k)
            (declare (ignore s2 k))
            (let ((len (length s1)))
              `(integer 0 ,len))))
  4)

(def-type-prop-test mismatch.11 'mismatch
  (list 'sequence 'sequence
        '(eql :start2)
        #'(lambda (s1 s2 k)
            (declare (ignore s1 k))
            (let ((len (length s2)))
              `(integer 0 ,len))))
  4)

(def-type-prop-test mismatch.12 'mismatch
  (list 'sequence 'sequence
        '(eql :end2)
        #'(lambda (s1 s2 k)
            (declare (ignore s1 k))
            (let ((len (length s2)))
              `(integer 0 ,len))))
  4)
