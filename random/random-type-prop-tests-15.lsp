;-*- Mode:     Lisp -*-

(in-package :cl-test)

(def-type-prop-test make-hash-table.1 '(lambda (x) (make-hash-table :test x))
  `((member eq eql equal equalp ,#'eq ,#'eql ,#'equal ,#'equalp))
  1
  :test #'equalp)

(def-type-prop-test make-hash-table.2 '(lambda (x) (make-hash-table :size x))
  '((integer 0 100000))
  1
  :test #'equalp)

(def-type-prop-test make-hash-table.3 '(lambda (x y)
                                        (let ((h (make-hash-table)))
                                          (setf (gethash x h) y)
                                          h))
  '(t t)
  2                  
  :test #'equalp)

(def-type-prop-test make-hash-table.4 '(lambda (x y z w)
                                        (let ((h (make-hash-table)))
                                          (setf (gethash x h) y)
                                          (setf (gethash z h) w)
                                          h))
  '(t t t t)
  4         
  :test #'equalp)

(def-type-prop-test make-hash-table.5 '(lambda (x y z w tst)
                                        (let ((h (make-hash-table :test tst)))
                                          (setf (gethash x h) y)
                                          (setf (gethash z h) w)
                                          h))
  `(t t t t
    (member eq eql equal equalp ,#'eq ,#'eql ,#'equal ,#'equalp))
  5         
  :test #'equalp)

(def-type-prop-test make-hash-table.6 '(lambda (x y z w s)
                                        (let ((h (make-hash-table :size s)))
                                          (setf (gethash x h) y)
                                          (setf (gethash z h) w)
                                          h))
  `(t t t t
    (integer 0 10000))
  5         
  :test #'equalp)

(def-type-prop-test hash-table-p.1 'hash-table-p '(t) 1)



