;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Mar 30 22:10:34 1998
;;;; Contains: Testing of CL Features related to "CONS", part 22

(in-package :cl-test)
(use-package :rt)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set-difference

(defun set-difference-with-check (x y &key (key 'no-key)
					   test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result (apply #'set-difference
			 x y
			 `(,@(unless (eq key 'no-key) `(:key ,key))
			   ,@(when test `(:test ,test))
			   ,@(when test-not `(:test-not ,test-not))))))  
      (cond
       ((and (check-scaffold-copy x xcopy)
	     (check-scaffold-copy y ycopy))
	result)
       (t
	'failed)))))

(defun check-set-difference (x y z &key (key #'identity)
					(test #'eql))
  (and
   (not (eq 'failed z))
   (every #'(lambda (e) (member e x :key key :test test)) z)
   (every #'(lambda (e) (or (member e y :key key :test test)
			    (member e z :key key :test test))) x)
   (every #'(lambda (e) (not (member e z :key key :test test))) y)
   t))

(deftest set-difference-1
    (set-difference nil nil)
  nil)

(deftest set-difference-2
    (let ((result
	   (set-difference-with-check '(a b c) nil)))
      (check-set-difference '(a b c) nil result))
  t)

(deftest set-difference-3
    (let ((result
	   (set-difference-with-check '(a b c d e f) '(f b d))))
      (check-set-difference '(a b c d e f) '(f b d) result))
  t)

(deftest set-difference-4
    (sort
     (copy-list
      (set-difference-with-check (shuffle '(1 2 3 4 5 6 7 8))
				 '(10 101 4 74 2 1391 7 17831)))
     #'<)
  (1 3 5 6 8))

(deftest set-difference-5
    (set-difference-with-check nil '(a b c d e f g h))
  nil)

(deftest set-difference-6
    (handler-case
	(set-difference-with-check '(a b c d e) '(d a b e)
				   :key nil)
      (error (c) c))
  (c))

(deftest set-difference-7
    (set-difference-with-check '(a b c d e) '(d a b e) :test #'eq)
  (c))

(deftest set-difference-8
    (set-difference-with-check '(a b c d e) '(d a b e) :test #'eql)
  (c))

(deftest set-difference-9
    (set-difference-with-check '(a b c d e) '(d a b e) :test #'equal)
  (c))

(deftest set-difference-10
    (set-difference-with-check '(a b c d e) '(d a b e)
			       :test 'eq)
  (c))

(deftest set-difference-11
    (set-difference-with-check '(a b c d e) '(d a b e)
			       :test 'eql)
  (c))

(deftest set-difference-12
    (set-difference-with-check '(a b c d e) '(d a b e)
			       :test 'equal)
  (c))

(defun do-random-set-differences (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
       for i from 1 to niters do
	  (let ((x (shuffle (loop for j from 1 to size collect
				  (random maxelem state))))
		(y (shuffle (loop for j from 1 to size collect
				  (random maxelem state)))))
	    (let ((z (set-difference-with-check x y)))
	      (let ((is-good (check-set-difference x y z)))
		(unless is-good (return (values x y z)))))))
    nil))

(deftest set-difference-13
    (do-random-set-differences 100 100)
  nil)

(deftest set-difference-14
    (set-difference-with-check '((a . 1) (b . 2) (c . 3))
			       '((a . 1) (c . 3))
			       :key 'car)
  ((b . 2)))

(deftest set-difference-15
    (set-difference-with-check '((a . 1) (b . 2) (c . 3))
			       '((a . 1) (c . 3))
			       :key #'car)
  ((b . 2)))

;;
;; Verify that the :test argument is called with the arguments
;; in the correct order
;;
(deftest set-difference-16
    (block fail
      (sort
	    (copy-list
	     (set-difference-with-check
	      '(1 2 3 4) '(e f g h)
	      :test #'(lambda (x y)
			(when (or (member x '(e f g h))
				  (member y '(1 2 3 4)))
			  (return-from fail 'fail))
			(eq x y))))
	    #'<))
  (1 2 3 4))

(deftest set-difference-17
    (block fail
      (sort
	    (copy-list
	     (set-difference-with-check
	      '(1 2 3 4) '(e f g h)
	      :key #'identity
	      :test #'(lambda (x y)
			(when (or (member x '(e f g h))
				  (member y '(1 2 3 4)))
			  (return-from fail 'fail))
			(eq x y))))
	    #'<))
  (1 2 3 4))

(deftest set-difference-18
    (block fail
      (sort
	    (copy-list
	     (set-difference-with-check
	      '(1 2 3 4) '(e f g h)
	      :test-not
	      #'(lambda (x y)
			(when (or (member x '(e f g h))
				  (member y '(1 2 3 4)))
			  (return-from fail 'fail))
			(not (eq x y)))))
	    #'<))
  (1 2 3 4))

(deftest set-difference-19
    (block fail
      (sort
	    (copy-list
	     (set-difference-with-check
	      '(1 2 3 4) '(e f g h)
	      :test-not
	      #'(lambda (x y)
			(when (or (member x '(e f g h))
				  (member y '(1 2 3 4)))
			  (return-from fail 'fail))
			(not (eq x y)))))
	    #'<))
  (1 2 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nset-difference

(defun nset-difference-with-check (x y &key (key 'no-key)
					   test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (apply #'nset-difference
	 x y
	 `(,@(unless (eq key 'no-key) `(:key ,key))
	     ,@(when test `(:test ,test))
	     ,@(when test-not `(:test-not ,test-not)))))

(defun check-nset-difference (x y z &key (key #'identity)
					(test #'eql))
  (and
   (every #'(lambda (e) (member e x :key key :test test)) z)
   (every #'(lambda (e) (or (member e y :key key :test test)
			    (member e z :key key :test test))) x)
   (every #'(lambda (e) (not (member e z :key key :test test))) y)))

(deftest nset-difference-1
    (nset-difference nil nil)
  nil)

(deftest nset-difference-2
    (let ((result
	   (nset-difference-with-check '(a b c) nil)))
      (check-nset-difference '(a b c) nil result))
  t)

(deftest nset-difference-3
    (let ((result
	   (nset-difference-with-check '(a b c d e f) '(f b d))))
      (check-nset-difference '(a b c d e f) '(f b d) result))
  t)

(deftest nset-difference-4
    (sort
     (copy-list
      (nset-difference-with-check (shuffle '(1 2 3 4 5 6 7 8))
				 '(10 101 4 74 2 1391 7 17831)))
     #'<)
  (1 3 5 6 8))

(deftest nset-difference-5
    (nset-difference-with-check nil '(a b c d e f g h))
  nil)

(deftest nset-difference-6
    (handler-case
	(nset-difference-with-check '(a b c d e) '(d a b e)
				    :key nil)
      (error (c) c))
  (c))

(deftest nset-difference-7
    (nset-difference-with-check '(a b c d e) '(d a b e) :test #'eq)
  (c))

(deftest nset-difference-8
    (nset-difference-with-check '(a b c d e) '(d a b e) :test #'eql)
  (c))

(deftest nset-difference-9
    (nset-difference-with-check '(a b c d e) '(d a b e) :test #'equal)
  (c))

(deftest nset-difference-10
    (nset-difference-with-check '(a b c d e) '(d a b e)
			       :test 'eq)
  (c))

(deftest nset-difference-11
    (nset-difference-with-check '(a b c d e) '(d a b e)
			       :test 'eql)
  (c))

(deftest nset-difference-12
    (nset-difference-with-check '(a b c d e) '(d a b e)
			       :test 'equal)
  (c))

(defun do-random-nset-differences (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
       for i from 1 to niters do
	  (let ((x (shuffle (loop for j from 1 to size collect
				  (random maxelem state))))
		(y (shuffle (loop for j from 1 to size collect
				  (random maxelem state)))))
	    (let ((z (nset-difference-with-check x y)))
	      (let ((is-good (check-nset-difference x y z)))
		(unless is-good (return (values x y z)))))))
    nil))

(deftest nset-difference-13
    (do-random-nset-differences 100 100)
  nil)

(deftest nset-difference-14
    (nset-difference-with-check '((a . 1) (b . 2) (c . 3))
			       '((a . 1) (c . 3))
			       :key 'car)
  ((b . 2)))

(deftest nset-difference-15
    (nset-difference-with-check '((a . 1) (b . 2) (c . 3))
			       '((a . 1) (c . 3))
			       :key #'car)
  ((b . 2)))

;;
;; Verify that the :test argument is called with the arguments
;; in the correct order
;;
(deftest nset-difference-16
    (block fail
      (sort
	    (copy-list
	     (nset-difference-with-check
	      '(1 2 3 4) '(e f g h)
	      :test #'(lambda (x y)
			(when (or (member x '(e f g h))
				  (member y '(1 2 3 4)))
			  (return-from fail 'fail))
			(eq x y))))
	    #'<))
  (1 2 3 4))

(deftest nset-difference-17
    (block fail
      (sort
	    (copy-list
	     (nset-difference-with-check
	      '(1 2 3 4) '(e f g h)
	      :key #'identity
	      :test #'(lambda (x y)
			(when (or (member x '(e f g h))
				  (member y '(1 2 3 4)))
			  (return-from fail 'fail))
			(eq x y))))
	    #'<))
  (1 2 3 4))

(deftest nset-difference-18
    (block fail
      (sort
	    (copy-list
	     (nset-difference-with-check
	      '(1 2 3 4) '(e f g h)
	      :test-not
	      #'(lambda (x y)
			(when (or (member x '(e f g h))
				  (member y '(1 2 3 4)))
			  (return-from fail 'fail))
			(not (eq x y)))))
	    #'<))
  (1 2 3 4))

(deftest nset-difference-19
    (block fail
      (sort (copy-list
	     (nset-difference-with-check
	      '(1 2 3 4) '(e f g h)
	      :test-not
	      #'(lambda (x y)
			(when (or (member x '(e f g h))
				  (member y '(1 2 3 4)))
			  (return-from fail 'fail))
			(not (eq x y)))))
	    #'<))
  (1 2 3 4))
