;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Apr  1 21:49:43 1998
;;;; Contains: Testing of CL Features related to "CONS", part 23

(in-package :cl-test)
(use-package :rt)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set-exclusive-or

(defun set-exclusive-or-with-check (x y &key (key 'no-key)
					   test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result (apply #'set-exclusive-or
			 x y
			 `(,@(unless (eqt key 'no-key) `(:key ,key))
			   ,@(when test `(:test ,test))
			   ,@(when test-not `(:test-not ,test-not))))))  
      (cond
       ((and (check-scaffold-copy x xcopy)
	     (check-scaffold-copy y ycopy))
	result)
       (t
	'failed)))))

(defun check-set-exclusive-or (x y z &key (key #'identity)
					(test #'eql))
  (and
   (not (eqt 'failed z))
   (every #'(lambda (e) (or (member e x :key key :test test)
		            (member e y :key key :test test)))
	  z)
   (every #'(lambda (e) (if (member e y :key key :test test)
			    (not (member e z :key key :test test))
			  (member e z :key key :test test)))
	  x)
   (every #'(lambda (e) (if (member e x :key key :test test)
			    (not (member e z :key key :test test))
			  (member e z :key key :test test)))
	  y)
   t))

(deftest set-exclusive-or-1
    (set-exclusive-or nil nil)
  nil)

(deftest set-exclusive-or-2
    (let ((result
	   (set-exclusive-or-with-check '(a b c) nil)))
      (check-set-exclusive-or '(a b c) nil result))
  t)

(deftest set-exclusive-or-3
    (let ((result
	   (set-exclusive-or-with-check '(a b c d e f) '(f b d))))
      (check-set-exclusive-or '(a b c d e f) '(f b d) result))
  t)

(deftest set-exclusive-or-4
    (sort
     (copy-list
      (set-exclusive-or-with-check (shuffle '(1 2 3 4 5 6 7 8))
				 '(10 101 4 74 2 1391 7 17831)))
     #'<)
  (1 3 5 6 8 10 74 101 1391 17831))

(deftest set-exclusive-or-5
    (check-set-exclusive-or
     nil
     '(a b c d e f g h)
     (set-exclusive-or-with-check nil '(a b c d e f g h)))
  t)

(deftest set-exclusive-or-6
    (handler-case
	(set-exclusive-or-with-check '(a b c d e) '(d a b e)
				   :key nil)
      (error (c) c))
  (c))

(deftest set-exclusive-or-7
    (set-exclusive-or-with-check '(a b c d e) '(d a b e) :test #'eq)
  (c))

(deftest set-exclusive-or-7-a
    (set-exclusive-or-with-check '(d a b e) '(a b c d e) :test #'eq)
  (c))

(deftest set-exclusive-or-8
    (set-exclusive-or-with-check '(a b c d e) '(d a b e) :test #'eql)
  (c))

(deftest set-exclusive-or-8-a
    (set-exclusive-or-with-check '(e d b a) '(a b c d e) :test #'eql)
  (c))

(deftest set-exclusive-or-8-b
    (set-exclusive-or-with-check '(a b c d e) '(d a b e)
				 :test-not (complement #'eql))
  (c))

(deftest set-exclusive-or-9
    (set-exclusive-or-with-check '(a b c d e) '(d a b e) :test #'equal)
  (c))

(deftest set-exclusive-or-10
    (set-exclusive-or-with-check '(a b c d e) '(d a b e)
			       :test 'eq)
  (c))

(deftest set-exclusive-or-11
    (set-exclusive-or-with-check '(a b c d e) '(d a b e)
			       :test 'eql)
  (c))

(deftest set-exclusive-or-12
    (set-exclusive-or-with-check '(a b c d e) '(d a b e)
			       :test 'equal)
  (c))

(defun do-random-set-exclusive-ors (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
       for i from 1 to niters do
	  (let ((x (shuffle (loop for j from 1 to size collect
				  (random maxelem state))))
		(y (shuffle (loop for j from 1 to size collect
				  (random maxelem state)))))
	    (let ((z (set-exclusive-or-with-check x y)))
	      (let ((is-good (check-set-exclusive-or x y z)))
		(unless is-good (return (values x y z)))))))
    nil))

(deftest set-exclusive-or-13
    (do-random-set-exclusive-ors 100 100)
  nil)

(deftest set-exclusive-or-14
    (set-exclusive-or-with-check '((a . 1) (b . 2) (c . 3012))
			       '((a . 10) (c . 3))
			       :key 'car)
  ((b . 2)))

(deftest set-exclusive-or-15
    (set-exclusive-or-with-check '((a . xx) (b . 2) (c . 3))
			       '((a . 1) (c . 3313))
			       :key #'car)
  ((b . 2)))

(deftest set-exclusive-or-16
    (set-exclusive-or-with-check '((a . xx) (b . 2) (c . 3))
			       '((a . 1) (c . 3313))
			       :key #'car
			       :test-not (complement #'eql))
  ((b . 2)))

;;
;; Check that set-exclusive-or does not invert
;; the order of the arguments to the test function
;;
(deftest set-exclusive-or-17
    (let ((list1 '(a b c d))
	  (list2 '(e f g h)))
      (block fail
	(not (not
	      (set-exclusive-or-with-check
	       list1 list2
	       :test #'(lambda (s1 s2)
			 (when (or (member s1 list2)
				   (member s2 list1))
			   (return-from fail 'failed))))))))
  t)

(deftest set-exclusive-or-17-a
    (let ((list1 '(a b c d))
	  (list2 '(e f g h)))
      (block fail
	(not (not
	      (set-exclusive-or-with-check
	       list1 list2
	       :key #'identity
	       :test #'(lambda (s1 s2)
			 (when (or (member s1 list2)
				   (member s2 list1))
			   (return-from fail 'failed))))))))
  t)

(deftest set-exclusive-or-18
    (let ((list1 '(a b c d))
	  (list2 '(e f g h)))
      (block fail
	(not (not
	      (set-exclusive-or-with-check
	       list1 list2
	       :test-not
	       #'(lambda (s1 s2)
			 (when (or (member s1 list2)
				   (member s2 list1))
			   (return-from fail 'failed))
			 t))))))
  t)

(deftest set-exclusive-or-18-a
    (let ((list1 '(a b c d))
	  (list2 '(e f g h)))
      (block fail
	(not (not
	      (set-exclusive-or-with-check
	       list1 list2
	       :key #'identity
	       :test-not
	       #'(lambda (s1 s2)
			 (when (or (member s1 list2)
				   (member s2 list1))
			   (return-from fail 'failed))
			 t))))))
  t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nset-exclusive-or

(defun nset-exclusive-or-with-check (x y &key (key 'no-key)
					   test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (apply #'nset-exclusive-or
	 x y
	 `(,@(unless (eqt key 'no-key) `(:key ,key))
	     ,@(when test `(:test ,test))
	     ,@(when test-not `(:test-not ,test-not)))))

(deftest nset-exclusive-or-1
    (nset-exclusive-or nil nil)
  nil)

(deftest nset-exclusive-or-2
    (let ((result
	   (nset-exclusive-or-with-check '(a b c) nil)))
      (check-set-exclusive-or '(a b c) nil result))
  t)

(deftest nset-exclusive-or-3
    (let ((result
	   (nset-exclusive-or-with-check '(a b c d e f) '(f b d))))
      (check-set-exclusive-or '(a b c d e f) '(f b d) result))
  t)

(deftest nset-exclusive-or-4
    (sort
     (copy-list
      (nset-exclusive-or-with-check (shuffle '(1 2 3 4 5 6 7 8))
				 '(10 101 4 74 2 1391 7 17831)))
     #'<)
  (1 3 5 6 8 10 74 101 1391 17831))

(deftest nset-exclusive-or-5
    (check-set-exclusive-or
     nil
     '(a b c d e f g h)
     (nset-exclusive-or-with-check nil '(a b c d e f g h)))
  t)

(deftest nset-exclusive-or-6
    (handler-case
	(nset-exclusive-or-with-check '(a b c d e) '(d a b e)
				   :key nil)
      (error (c) c))
  (c))

(deftest nset-exclusive-or-7
    (nset-exclusive-or-with-check '(a b c d e) '(d a b e) :test #'eq)
  (c))

(deftest nset-exclusive-or-7-a
    (nset-exclusive-or-with-check '(d a b e) '(a b c d e) :test #'eq)
  (c))

(deftest nset-exclusive-or-8
    (nset-exclusive-or-with-check '(a b c d e) '(d a b e) :test #'eql)
  (c))

(deftest nset-exclusive-or-8-a
    (nset-exclusive-or-with-check '(e d b a) '(a b c d e) :test #'eql)
  (c))

(deftest nset-exclusive-or-8-b
    (nset-exclusive-or-with-check '(a b c d e) '(d a b e)
				  :test-not (complement #'eql))
  (c))

(deftest nset-exclusive-or-9
    (nset-exclusive-or-with-check '(a b c d e) '(d a b e) :test #'equal)
  (c))

(deftest nset-exclusive-or-10
    (nset-exclusive-or-with-check '(a b c d e) '(d a b e)
			       :test 'eq)
  (c))

(deftest nset-exclusive-or-11
    (nset-exclusive-or-with-check '(a b c d e) '(d a b e)
			       :test 'eql)
  (c))

(deftest nset-exclusive-or-12
    (nset-exclusive-or-with-check '(a b c d e) '(d a b e)
			       :test 'equal)
  (c))

(defun do-random-nset-exclusive-ors (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
       for i from 1 to niters do
	  (let ((x (shuffle (loop for j from 1 to size collect
				  (random maxelem state))))
		(y (shuffle (loop for j from 1 to size collect
				  (random maxelem state)))))
	    (let ((z (nset-exclusive-or-with-check x y)))
	      (let ((is-good (check-set-exclusive-or x y z)))
		(unless is-good (return (values x y z)))))))
    nil))

(deftest nset-exclusive-or-13
    (do-random-nset-exclusive-ors 100 100)
  nil)

(deftest nset-exclusive-or-14
    (nset-exclusive-or-with-check '((a . 1) (b . 2) (c . 3012))
			       '((a . 10) (c . 3))
			       :key 'car)
  ((b . 2)))

(deftest nset-exclusive-or-15
    (nset-exclusive-or-with-check '((a . xx) (b . 2) (c . 3))
			       '((a . 1) (c . 3313))
			       :key #'car)
  ((b . 2)))

(deftest nset-exclusive-or-16
    (nset-exclusive-or-with-check '((a . xx) (b . 2) (c . 3))
			       '((a . 1) (c . 3313))
			       :key #'car
			       :test-not (complement #'eql))
  ((b . 2)))

;;
;; Check that nset-exclusive-or does not invert
;; the order of the arguments to the test function
;;
(deftest nset-exclusive-or-17
    (let ((list1 '(a b c d))
	  (list2 '(e f g h)))
      (block fail
	(not (not
	      (nset-exclusive-or-with-check
	       list1 list2
	       :test #'(lambda (s1 s2)
			 (when (or (member s1 list2)
				   (member s2 list1))
			   (return-from fail 'failed))))))))
  t)

(deftest nset-exclusive-or-17-a
    (let ((list1 '(a b c d))
	  (list2 '(e f g h)))
      (block fail
	(not (not
	      (nset-exclusive-or-with-check
	       list1 list2
	       :key #'identity
	       :test #'(lambda (s1 s2)
			 (when (or (member s1 list2)
				   (member s2 list1))
			   (return-from fail 'failed))))))))
  t)

(deftest nset-exclusive-or-18
    (let ((list1 '(a b c d))
	  (list2 '(e f g h)))
      (block fail
	(not (not
	      (nset-exclusive-or-with-check
	       list1 list2
	       :test-not
	       #'(lambda (s1 s2)
			 (when (or (member s1 list2)
				   (member s2 list1))
			   (return-from fail 'failed))
			 t))))))
  t)

(deftest nset-exclusive-or-18-a
    (let ((list1 '(a b c d))
	  (list2 '(e f g h)))
      (block fail
	(not (not
	      (nset-exclusive-or-with-check
	       list1 list2
	       :key #'identity
	       :test-not
	       #'(lambda (s1 s2)
			 (when (or (member s1 list2)
				   (member s2 list1))
			   (return-from fail 'failed))
			 t))))))
  t)
