;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Contains: Random type prop tests, part 10 (sequences, cont.)

(in-package :cl-test)

;;; SEARCH

(def-type-prop-test search.1 'search
  (list 'sequence 'sequence)
  2)

(def-type-prop-test search.2 'search
  (list 'bit-vector 'bit-vector)
  2)

(def-type-prop-test search.3 'search
  (list '(vector * 1) 'sequence)
  2)

(def-type-prop-test search.4 'search
  (list '(vector * 2) 'sequence '(eql :from-end) '(or null t))
  4)

(def-type-prop-test search.5 'search
  (list 'sequence 'sequence '(eql :key)
        (list 'member 'identity nil #'identity 'not #'not))
  4)

(def-type-prop-test search.6 'search
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

(def-type-prop-test search.7 'search
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

(def-type-prop-test search.8 'search
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

(def-type-prop-test search.9 'search
  (list 'sequence 'sequence
        '(eql :start1)
        #'(lambda (s1 s2 k)
            (declare (ignore s2 k))
            (let ((len (length s1)))
              `(integer 0 ,len))))
  4)

(def-type-prop-test search.10 'search
  (list 'sequence 'sequence
        '(eql :end1)
        #'(lambda (s1 s2 k)
            (declare (ignore s2 k))
            (let ((len (length s1)))
              `(integer 0 ,len))))
  4)

(def-type-prop-test search.11 'search
  (list 'sequence 'sequence
        '(eql :start2)
        #'(lambda (s1 s2 k)
            (declare (ignore s1 k))
            (let ((len (length s2)))
              `(integer 0 ,len))))
  4)

(def-type-prop-test search.12 'search
  (list 'sequence 'sequence
        '(eql :end2)
        #'(lambda (s1 s2 k)
            (declare (ignore s1 k))
            (let ((len (length s2)))
              `(integer 0 ,len))))
  4)

(def-type-prop-test search.13 '(lambda (seq1 seq2 s1 e1)
                                (search seq1 seq2 :start1 s1 :end1 e1))
  (list 'sequence 'sequence
        #'(lambda (seq1 seq2)
            (declare (ignore seq2))
            `(integer 0 ,(length seq1)))
        #'(lambda (seq1 seq2 s1)
            (declare (ignore seq2))
            `(or null (integer ,s1 ,(length seq1)))))
  4)

(def-type-prop-test search.14 '(lambda (seq1 seq2 s1 e1 s2)
                                (search seq1 seq2 :start1 s1 :end1 e1 :start2 s2))
  (list 'sequence 'sequence
        #'(lambda (seq1 seq2)
            (declare (ignore seq2))
            `(integer 0 ,(length seq1)))
        #'(lambda (seq1 seq2 s1)
            (declare (ignore seq2))
            `(or null (integer ,s1 ,(length seq1))))
        #'(lambda (seq1 seq2 s1 e1)
            (declare (ignore seq1 s1 e1))
            `(integer 0 ,(length seq2))))
  5)

(def-type-prop-test search.15 '(lambda (seq1 seq2 s1 e1 s2 e2)
                                (search seq1 seq2 :start1 s1 :end1 e1 :start2 s2 :end2 e2))
  (list 'sequence 'sequence
        #'(lambda (seq1 seq2)
            (declare (ignore seq2))
            `(integer 0 ,(length seq1)))
        #'(lambda (seq1 seq2 s1)
            (declare (ignore seq2))
            `(or null (integer ,s1 ,(length seq1))))
        #'(lambda (seq1 seq2 s1 e1)
            (declare (ignore seq1 s1 e1))
            `(integer 0 ,(length seq2)))
        #'(lambda (seq1 seq2 s1 e1 s2)
            (declare (ignore seq1 s1 e1))
            `(or null (integer ,s2 ,(length seq2)))))
  6)

(def-type-prop-test search.16 '(lambda (seq1 seq2 s1 s2 e2)
                                (search seq1 seq2 :start1 s1 :start2 s2 :end2 e2))
  (list 'sequence 'sequence
        #'(lambda (seq1 seq2)
            (declare (ignore seq2))
            `(integer 0 ,(length seq1)))
        #'(lambda (seq1 seq2 s1)
            (declare (ignore seq1 s1))
            `(integer 0 ,(length seq2)))
        #'(lambda (seq1 seq2 s1 s2)
            (declare (ignore seq1 s1))
            `(or null (integer ,s2 ,(length seq2)))))
  5)

(def-type-prop-test search.17 '(lambda (seq1 seq2 e1 s2 e2)
                                (search seq1 seq2 :end1 e1 :start2 s2 :end2 e2))
  (list 'sequence 'sequence
        #'(lambda (seq1 seq2)
            (declare (ignore seq2))
            `(integer 0 ,(length seq1)))
        #'(lambda (seq1 seq2 e1)
            (declare (ignore seq1 e1))
            `(integer 0 ,(length seq2)))
        #'(lambda (seq1 seq2 e1 s2)
            (declare (ignore seq1 e1))
            `(or null (integer ,s2 ,(length seq2)))))
  5)


     
