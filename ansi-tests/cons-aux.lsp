;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Mar  6 17:45:42 2003
;;;; Contains: Auxiliary functions for cons-related tests

(in-package :cl-test)

(defun my-set-exclusive-or (set1 set2 &key key test test-not)

  (assert (not (and test test-not)))

  (cond
   (test-not (when (symbolp test-not)
	       (setq test-not (symbol-function test-not)))
	     (setq test (complement test-not)))
   ((not test) (setq test #'eql)))

  (when (symbolp test) (setq test (symbol-function test)))	  

  (let* ((keys1 (if key (mapcar key set1) set1))
	 (keys2 (if key (mapcar key set2) set2))
	 (mask1 (make-array (length set1) :element-type 'bit
			    :initial-element 0))
	 (mask2 (make-array (length set2) :element-type 'bit
			    :initial-element 0)))
    (loop for i1 from 0
	  for k1 in keys1
	  do
	  (loop for i2 from 0
		for k2 in keys2
		when (funcall test k1 k2)
		do (setf (sbit mask1 i1) 1
			 (sbit mask2 i2) 1)))
    (nconc
     (loop for e in set1
	   for i across mask1
	   when (= i 0)
	   collect e)
     (loop for e in set2
	   for i across mask2
	   when (= i 0)
	   collect e))))

(defun make-random-set-exclusive-or-input (n)
  (let* ((set1 (loop for i from 1 to n collect (random n)))
	 (set2 (loop for i from 1 to n collect (random n)))
	 (test-args
	  (random-case nil nil nil
		       (list :test 'eql)
		       (list :test #'eql)
		       (list :test (complement #'eql))))
	 (test-not-args
	  (and (not test-args)
	       (random-case nil nil (list :test-not 'eql)
			    (list :test-not #'eql)
			    (list :test-not (complement #'eql)))))
	 (key-args
	  (random-case nil nil nil nil
		       (list :key nil)
		       (list :key 'identity)
		       (list :key 'not))))
    (list* set1 set2
	  (reduce #'append (random-permute
			    (list test-args test-not-args key-args))))))

(defun random-set-exclusive-or-test (n reps &optional (fn 'set-exclusive-or))
  (loop for i below reps
	for args = (make-random-set-exclusive-or-input n)
	for set1 = (car args)
	for set2 = (cadr args)
	for result1 = (apply #'remove-duplicates
			     (sort (copy-list (apply #'my-set-exclusive-or args))
				   #'<)
			     (cddr args))
	for result2 = (apply #'remove-duplicates
			     (sort (copy-list (apply fn
						     (copy-list set1)
						     (copy-list set2)
						     (cddr args)))
				   #'<)
			     (cddr args))
	unless (equal result1 result2)
	return (list (list 'remove-duplicates (list 'sort (cons fn args) '<) "...")
		     "actual: " result2 "should be: " result1)))

(defun rev-assoc-list (x)
  (cond
   ((null x) nil)
   ((null (car x))
    (cons nil (rev-assoc-list (cdr x))))
   (t
    (acons (cdar x) (caar x) (rev-assoc-list (cdr x))))))
