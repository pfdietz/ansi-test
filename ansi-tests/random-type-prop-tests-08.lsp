;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Mar 13 18:31:57 2005
;;;; Contains: Random type prop tests, part 8 (sequences)

(in-package :cl-test)

(def-type-prop-test copy-seq 'copy-seq '((or vector list)) 1)

(def-type-prop-test elt 'elt (list '(or vector list)
				   #'(lambda (x) (let ((len (length x)))
						   (and (> len 0) `(integer 0 (,len))))))
  2)
