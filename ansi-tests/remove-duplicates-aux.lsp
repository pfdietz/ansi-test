;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep 23 20:59:10 2002
;;;; Contains: Aux. functions for testing REMOVE-DUPLICATES/DELETE-DUPLICATES

(in-package :cl-test)

(defun my-remove-duplicates (orig-sequence
			     &key from-end test test-not (start 0) end key)
  (assert (typep orig-sequence 'sequence))
  (let* ((sequence orig-sequence)
	 (len (length sequence)))
    (unless end (setq end len))
    (unless key (setq key #'identity))
    (cond
      (test (assert (not test-not)))
      (test-not (setq test #'(lambda (x y) (not (funcall test x y)))))
      (t (setq test #'eql)))
    (assert (integerp start))
    (assert (integerp end))
    (assert (<= 0 start end len))
    (format t "start = ~A, end = ~A, len = ~A~%" start end len)
    (if from-end
	(psetq start (- len end)
	       end (- len start)
	       sequence (reverse sequence))
	(setq sequence (copy-seq sequence)))
    (format t "start = ~A, end = ~A, len = ~A~%" start end len)
    (assert (<= 0 start end len) (start end len))
    (let ((result nil))
      (loop for i from 0 below start
	    do (push (elt sequence i) result))
      (loop for i from start below end
	    for x = (elt sequence i)
	    unless (position x
			     sequence
			     :start (1+ i)
			     :end end
			     :test test
			     :key key)
	    do (push x result))
      (loop for i from end below len
	    do (push (elt sequence i) result))
      (unless from-end (setq result (reverse result)))
      (cond
	((listp orig-sequence) result)
	((arrayp orig-sequence)
	 (make-array (length result) :initial-contents result
		     :element-type (array-element-type orig-sequence)))
	(t (assert nil))))))


