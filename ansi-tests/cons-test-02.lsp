;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:30:50 1998
;;;; Contains: Testing of CL Features related to "CONS", part 2

(in-package :cl-test)
(use-package :rt)

(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; copy-tree

(defun check-cons-copy (x y)
  "Check that the tree x is a copy of the tree y,
   returning t iff it is."
  (cond
   ((consp x)
    (and (consp y)
	 (not (eq x y))
	 (check-cons-copy (car x) (car y))
	 (check-cons-copy (cdr x) (cdr y))))
   ((eq x y) t)
   (t nil)))

;; Try copy-tree on a tree containing elements of various kinds
(deftest copy-tree-1
    (let ((x (cons 'a (list (cons 'b 'c)
			    (cons 1 1.2)
			    (list (list "abcde"
					(make-array '(10) :initial-element (cons 'e 'f)))
				  'g)))))
      (let ((y (copy-tree x)))
	(check-cons-copy x y)))
  t)

;; Try copy-tree on *universe*
(deftest copy-tree-2
    (let ((x (copy-list *universe*)))
      (let ((y (copy-tree x)))
	(check-cons-copy x y)))
  t)

;; Check sublis

(defun check-sublis (a al &key (key 'no-key) test test-not)
  "Apply sublis al a with various keys.  Check that
   the arguments are not themselves changed.  Return nil
   if the arguments do get changed."
  (setf a (copy-tree a))
  (setf al (copy-tree al))
  (let ((acopy (make-scaffold-copy a))
	(alcopy (make-scaffold-copy al)))
    (let ((as
	   (apply #'sublis al a
		  `(,@(when test `(:test ,test))
		    ,@(when test-not `(:test-not ,test-not))
		    ,@(unless (eq key 'no-key) `(:key ,key))))))
      (and
       (check-scaffold-copy a acopy)
       (check-scaffold-copy al alcopy)
       as))))

(deftest sublis-1
    (check-sublis '((a b) g (d e 10 g h) 15 . g)
		  '((e . e2) (g . 17)))
  ((a b) 17 (d e2 10 17 h) 15 . 17))

(deftest sublis-2
    (check-sublis '(f6 10 (f4 (f3 (f1 a b) (f1 a p)) (f2 a b)))
		  '(((f1 a b) . (f2 a b)) ((f2 a b) . (f1 a b)))
		  :test #'equal)
  (F6 10 (F4 (F3 (F2 A B) (F1 A P)) (F1 A B))))

(deftest sublis-3
    (check-sublis '(10 ((10 20 (a b c) 30)) (((10 20 30 40))))
		  '((30 . "foo")))
  (10 ((10 20 (a b c) "foo")) (((10 20 "foo" 40)))))

(deftest sublis-4
    (check-sublis (sublis
		   (copy-tree '((a . 2) (b . 4) (c . 1)))
		   (copy-tree '(a b c d e (a b c a d b) f)))
		  '((t . "yes"))
		  :key #'(lambda (x) (and (typep x 'integer)
					  (evenp x))))
  ("yes" "yes" 1 d e ("yes" "yes" 1 "yes" d "yes") f))

(deftest sublis-5
    (check-sublis '("fee" (("fee" "Fie" "foo"))
		    fie ("fee" "fie"))
		  `((,(copy-seq "fie") . #\f)))
  ("fee" (("fee" "Fie" "foo")) fie ("fee" "fie")))

(deftest sublis-6
    (check-sublis '("fee" fie (("fee" "Fie" "foo") 1)
		    ("fee" "fie"))
		  `((,(copy-seq "fie") . #\f))
		  :test 'equal)
  ("fee" fie (("fee" "Fie" "foo") 1) ("fee" #\f)))

(deftest sublis-7
    (check-sublis '(("aa" a b)
		    (z "bb" d)
		    ((x . "aa")))
		  `((,(copy-seq "aa") . 1)
		    (,(copy-seq "bb") . 2))
		  :test 'equal
		  :key #'(lambda (x) (if (consp x) (car x)
				       '*not-present*)))
  (1 (z . 2) ((x . "aa"))))

;; Check that a null key arg is ignored.

(deftest sublis-8
  (handler-case
      (check-sublis 
       '(1 2 a b)
       '((1 . 2) (a . b))
       :key nil)
   (error (c) c))
  (2 2 b b))

;; nsublis

(defun check-nsublis (a al &key (key 'no-key) test test-not)
  "Apply nsublis al a, copying these arguments first."
  (setf a (copy-tree a))
  (setf al (copy-tree al))
  (let ((as
	 (apply #'sublis (copy-tree al) (copy-tree a)
		`(,@(when test `(:test ,test))
		    ,@(when test-not `(:test-not ,test-not))
		    ,@(unless (eq key 'no-key) `(:key ,key))))))
    as))


(deftest nsublis-1
    (check-nsublis '((a b) g (d e 10 g h) 15 . g)
		  '((e . e2) (g . 17)))
  ((a b) 17 (d e2 10 17 h) 15 . 17))

(deftest nsublis-2
    (check-nsublis '(f6 10 (f4 (f3 (f1 a b) (f1 a p)) (f2 a b)))
		  '(((f1 a b) . (f2 a b)) ((f2 a b) . (f1 a b)))
		  :test #'equal)
  (F6 10 (F4 (F3 (F2 A B) (F1 A P)) (F1 A B))))

(deftest nsublis-3
    (check-nsublis '(10 ((10 20 (a b c) 30)) (((10 20 30 40))))
		  '((30 . "foo")))
  (10 ((10 20 (a b c) "foo")) (((10 20 "foo" 40)))))

(deftest nsublis-4
    (check-nsublis
		  (nsublis (copy-tree '((a . 2) (b . 4) (c . 1)))
			   (copy-tree '(a b c d e (a b c a d b) f)))
		  '((t . "yes"))
		  :key #'(lambda (x) (and (typep x 'integer)
					  (evenp x))))
  ("yes" "yes" 1 d e ("yes" "yes" 1 "yes" d "yes") f))

(deftest nsublis-5
    (check-nsublis '("fee" (("fee" "Fie" "foo"))
		    fie ("fee" "fie"))
		  `((,(copy-seq "fie") . #\f)))
  ("fee" (("fee" "Fie" "foo")) fie ("fee" "fie")))

(deftest nsublis-6
    (check-nsublis '("fee" fie (("fee" "Fie" "foo") 1)
		    ("fee" "fie"))
		  `((,(copy-seq "fie") . #\f))
		  :test 'equal)
  ("fee" fie (("fee" "Fie" "foo") 1) ("fee" #\f)))

(deftest nsublis-7
    (check-nsublis '(("aa" a b)
		    (z "bb" d)
		    ((x . "aa")))
		  `((,(copy-seq "aa") . 1)
		    (,(copy-seq "bb") . 2))
		  :test 'equal
		  :key #'(lambda (x) (if (consp x) (car x)
				       '*not-present*)))
  (1 (z . 2) ((x . "aa"))))

;; Check that a null key arg is ignored.

(deftest nsublis-8
  (handler-case
      (check-nsublis 
       '(1 2 a b)
       '((1 . 2) (a . b))
       :key nil)
   (error (c) c))
  (2 2 b b))

(deftest sublis-shared
    (let* ((shared-piece (list 'a 'b))
	   (a (list shared-piece shared-piece)))
      (check-sublis a '((a . b) (b . a))))
  ((b a) (b a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check subst

(defun check-subst (new old tree &key (key 'no-key) test test-not)
  "Call subst new old tree, with keyword arguments if present.
   Check that the arguments are not changed."
  (setf new (copy-tree new))
  (setf old (copy-tree old))
  (setf tree (copy-tree tree))
  (let ((newcopy (make-scaffold-copy new))
	(oldcopy (make-scaffold-copy old))
	(treecopy (make-scaffold-copy tree)))
    (let ((result
	   (apply #'subst new old tree
		  `(,@(unless (eq key 'no-key) `(:key ,key))
		    ,@(when test `(:test ,test))
		    ,@(when test-not `(:test-not ,test-not))))))
      (and (check-scaffold-copy new newcopy)
	   (check-scaffold-copy old oldcopy)
	   (check-scaffold-copy tree treecopy)
	   result))))


(defvar *subst-tree-1* '(10 (30 20 10) (20 10) (10 20 30 40)))

(deftest subst-1
    (check-subst "Z" 30 (copy-tree *subst-tree-1*))
  (10 ("Z" 20 10) (20 10) (10 20 "Z" 40)))

(deftest subst-2
    (check-subst "A" 0 (copy-tree *subst-tree-1*))
  (10 (30 20 10) (20 10) (10 20 30 40)))

(deftest subst-3
    (check-subst "Z" 100 (copy-tree *subst-tree-1*) :test-not #'eql)
  "Z")

(deftest subst-4
    (check-subst 'grape 'dick
		 '(melville wrote (moby dick)))
  (MELVILLE WROTE (MOBY GRAPE)))

(deftest subst-5
    (check-subst 'cha-cha-cha 'nil '(melville wrote (moby dick)))
  (MELVILLE WROTE (MOBY DICK . CHA-CHA-CHA) . CHA-CHA-CHA))

(deftest subst-6
    (check-subst
     '(1 2) '(foo . bar)
     '((foo . baz) (foo . bar) (bar . foo) (baz foo . bar))
     :test #'equal)
  ((foo . baz) (1 2) (bar . foo) (baz 1 2)))

(deftest subst-7
    (check-subst
     'foo "aaa"
     '((1 . 2) (4 . 5) (6 7 8 9 10 (11 12)))
     :key #'(lambda (x) (if (and (numberp x) (evenp x))
			    "aaa"
			  nil))
     :test #'string=)
  ((1 . foo) (foo . 5) (foo 7 foo 9 foo (11 foo))))

(deftest subst-8
    (check-subst
     'foo nil
     '((1 . 2) (4 . 5) (6 7 8 9 10 (11 12)))
     :key #'(lambda (x) (if (and (numberp x) (evenp x))
			    (copy-seq "aaa")
			  nil))
     :test-not #'equal)
  ((1 . foo) (foo . 5) (foo 7 foo 9 foo (11 foo))))

(deftest subst-9
  (handler-case
   (check-subst 'a 'b
		(copy-tree '(a b c d a b))
		:key nil)
   (error (c) c))
  (a a c d a a))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check subst-if, subst-if-not

(defun check-subst-if (new pred tree &key (key 'no-key))
  "Call subst-if new pred tree, with various keyword arguments
   if present.  Check that the arguments are not changed."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (let ((newcopy (make-scaffold-copy new))
	(predcopy (make-scaffold-copy pred))
	(treecopy (make-scaffold-copy tree)))
    (let ((result
	   (apply #'subst-if new pred tree
		  (unless (eq key 'no-key) `(:key ,key)))))
      (and (check-scaffold-copy new newcopy)
	   (check-scaffold-copy pred predcopy)
	   (check-scaffold-copy tree treecopy)
	   result))))

(defun check-subst-if-not (new pred tree &key (key 'no-key))
  "Call subst-if-not new pred tree, with various keyword arguments
   if present.  Check that the arguments are not changed."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (let ((newcopy (make-scaffold-copy new))
	(predcopy (make-scaffold-copy pred))
	(treecopy (make-scaffold-copy tree)))
    (let ((result
	   (apply #'subst-if-not new pred tree
		  (unless (eq key 'no-key) `(:key ,key)))))
      (and (check-scaffold-copy new newcopy)
	   (check-scaffold-copy pred predcopy)
	   (check-scaffold-copy tree treecopy)
	   result))))

(deftest subst-if-1
    (check-subst-if 'a #'consp '((100 1) (2 3) (4 3 2 1) (a b c)))
  A)

(deftest subst-if-not-1
    (check-subst-if-not '(x) 'consp '(1 (1 2) (1 2 3) (1 2 3 4)))
  ((X)
   ((X) (X) X)
   ((X) (X) (X) X)
   ((X) (X) (X) (X) X)
   X))

(deftest subst-if-2
    (check-subst-if 17 (complement #'listp) '(a (a b) (a c d) (a nil e f g)))
  (17 (17 17) (17 17 17) (17 nil 17 17 17)))

(deftest subst-if-3
    (check-subst-if '(z)
		    (complement #'consp)
		    '(a (a b) (c d e) (f g h i)))
  ((Z)
   ((Z) (Z) Z)
   ((Z) (Z) (Z) Z)
   ((Z) (Z) (Z) (Z) Z)
   Z))

(deftest subst-if-not-2
    (check-subst-if-not 'a (complement #'listp)
			'((100 1) (2 3) (4 3 2 1) (a b c)))
  A)

(deftest subst-if-4
    (check-subst-if 'b #'identity '((100 1) (2 3) (4 3 2 1) (a b c))
		    :key #'listp)
  B)

(deftest subst-if-not-3
    (check-subst-if-not 'c #'identity
			'((100 1) (2 3) (4 3 2 1) (a b c))
			:key (complement #'listp))
  C)

(deftest subst-if-5
    (check-subst-if 4 #'(lambda (x) (eql x 1))
		    '((1 3) (1) (1 10 20 30) (1 3 x y))
		    :key #'(lambda (x)
			     (and (consp x)
				  (car x))))
  (4 4 4 4))

(deftest subst-if-not-4
    (check-subst-if-not
     40
     #'(lambda (x) (not (eql x 17)))
     '((17) (17 22) (17 22 31) (17 21 34 54))
     :key #'(lambda (x)
	      (and (consp x)
		   (car x))))
  (40 40 40 40))

(deftest subst-if-6
  (handler-case
   (check-subst-if 'a  #'(lambda (x) (eql x 'b))
		   '((a) (b) (c) (d))
		   :key nil)
   (error (c) c))
((a) (a) (c) (d)))
  
(deftest subst-if-not-5
  (handler-case
   (check-subst-if-not 'a  #'(lambda (x) (not (eql x 'b)))
		       '((a) (b) (c) (d))
		       :key nil)
   (error (c) c))
((a) (a) (c) (d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check nsubst

(defun check-nsubst (new old tree &key (key 'no-key) test test-not)
  "Call nsubst new old tree, with keyword arguments if present."
  (setf new (copy-tree new))
  (setf old (copy-tree old))
  (setf tree (copy-tree tree))
  (apply #'nsubst new old tree
	 `(,@(unless (eq key 'no-key) `(:key ,key))
	     ,@(when test `(:test ,test))
	     ,@(when test-not `(:test-not ,test-not)))))

(defvar *nsubst-tree-1* '(10 (30 20 10) (20 10) (10 20 30 40)))

(deftest nsubst-1
    (check-nsubst "Z" 30 (copy-tree *nsubst-tree-1*))
  (10 ("Z" 20 10) (20 10) (10 20 "Z" 40)))

(deftest nsubst-2
    (check-nsubst "A" 0 (copy-tree *nsubst-tree-1*))
  (10 (30 20 10) (20 10) (10 20 30 40)))

(deftest nsubst-3
    (check-nsubst "Z" 100 (copy-tree *nsubst-tree-1*) :test-not #'eql)
  "Z")

(deftest nsubst-4
    (check-nsubst 'grape 'dick
		 '(melville wrote (moby dick)))
  (MELVILLE WROTE (MOBY GRAPE)))

(deftest nsubst-5
    (check-nsubst 'cha-cha-cha 'nil '(melville wrote (moby dick)))
  (MELVILLE WROTE (MOBY DICK . CHA-CHA-CHA) . CHA-CHA-CHA))

(deftest nsubst-6
    (check-nsubst
     '(1 2) '(foo . bar)
     '((foo . baz) (foo . bar) (bar . foo) (baz foo . bar))
     :test #'equal)
  ((foo . baz) (1 2) (bar . foo) (baz 1 2)))

(deftest nsubst-7
    (check-nsubst
     'foo "aaa"
     '((1 . 2) (4 . 5) (6 7 8 9 10 (11 12)))
     :key #'(lambda (x) (if (and (numberp x) (evenp x))
			    "aaa"
			  nil))
     :test #'string=)
  ((1 . foo) (foo . 5) (foo 7 foo 9 foo (11 foo))))

(deftest nsubst-8
    (check-nsubst
     'foo nil
     '((1 . 2) (4 . 5) (6 7 8 9 10 (11 12)))
     :key #'(lambda (x) (if (and (numberp x) (evenp x))
			    (copy-seq "aaa")
			  nil))
     :test-not #'equal)
  ((1 . foo) (foo . 5) (foo 7 foo 9 foo (11 foo))))

(deftest nsubst-9
  (handler-case
   (check-nsubst 'a 'b
		(copy-tree '(a b c d a b))
		:key nil)
   (error (c) c))
  (a a c d a a))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check nsubst-if, nsubst-if-not

(defun check-nsubst-if (new pred tree &key (key 'no-key))
  "Call nsubst-if new pred tree, with keyword arguments if present."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (apply #'nsubst-if new pred tree
	 (unless (eq key 'no-key) `(:key ,key))))

(defun check-nsubst-if-not (new pred tree &key (key 'no-key))
  "Call nsubst-if-not new pred tree, with keyword arguments if present."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (apply #'nsubst-if-not new pred tree
		  (unless (eq key 'no-key) `(:key ,key))))

(deftest nsubst-if-1
    (check-nsubst-if 'a #'consp '((100 1) (2 3) (4 3 2 1) (a b c)))
  A)

(deftest nsubst-if-not-1
    (check-nsubst-if-not '(x) 'consp '(1 (1 2) (1 2 3) (1 2 3 4)))
  ((X)
   ((X) (X) X)
   ((X) (X) (X) X)
   ((X) (X) (X) (X) X)
   X))

(deftest nsubst-if-2
    (check-nsubst-if 17 (complement #'listp) '(a (a b) (a c d) (a nil e f g)))
  (17 (17 17) (17 17 17) (17 nil 17 17 17)))

(deftest nsubst-if-3
    (check-nsubst-if '(z)
		    (complement #'consp)
		    '(a (a b) (c d e) (f g h i)))
  ((Z)
   ((Z) (Z) Z)
   ((Z) (Z) (Z) Z)
   ((Z) (Z) (Z) (Z) Z)
   Z))

(deftest nsubst-if-not-2
    (check-nsubst-if-not 'a (complement #'listp)
			'((100 1) (2 3) (4 3 2 1) (a b c)))
  A)

(deftest nsubst-if-4
    (check-nsubst-if 'b #'identity '((100 1) (2 3) (4 3 2 1) (a b c))
		    :key #'listp)
  B)

(deftest nsubst-if-not-3
    (check-nsubst-if-not 'c #'identity
			'((100 1) (2 3) (4 3 2 1) (a b c))
			:key (complement #'listp))
  C)

(deftest nsubst-if-5
    (check-nsubst-if 4 #'(lambda (x) (eql x 1))
		    '((1 3) (1) (1 10 20 30) (1 3 x y))
		    :key #'(lambda (x)
			     (and (consp x)
				  (car x))))
  (4 4 4 4))

(deftest nsubst-if-not-4
    (check-nsubst-if-not
     40
     #'(lambda (x) (not (eql x 17)))
     '((17) (17 22) (17 22 31) (17 21 34 54))
     :key #'(lambda (x)
	      (and (consp x)
		   (car x))))
  (40 40 40 40))

(deftest nsubst-if-6
  (handler-case
   (check-nsubst-if 'a  #'(lambda (x) (eql x 'b))
		   '((a) (b) (c) (d))
		   :key nil)
   (error (c) c))
((a) (a) (c) (d)))
  
(deftest nsubst-if-not-5
  (handler-case
   (check-nsubst-if-not 'a  #'(lambda (x) (not (eql x 'b)))
		       '((a) (b) (c) (d))
		       :key nil)
   (error (c) c))
((a) (a) (c) (d)))
