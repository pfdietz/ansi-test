;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 09:45:22 1998
;;;; Contains: Testing of CL Features related to "CONS", part 17

(in-package :cl-test)
(use-package :rt)
(declaim (optimize (safety 3)))

(defun rev-assoc-list (x)
  (cond
   ((null x) nil)
   ((null (car x))
    (cons nil (rev-assoc-list (cdr x))))
   (t
    (acons (cdar x) (caar x) (rev-assoc-list (cdr x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; assoc

(deftest rassoc-1
    (rassoc nil nil)
  nil)

(deftest rassoc-2
    (rassoc nil '(nil))
  nil)

(deftest rassoc-3
    (rassoc nil (rev-assoc-list '(nil (nil . 2) (a . b))))
  (2 . nil))

(deftest rassoc-4
    (rassoc nil '((a . b) (c . d)))
  nil)

(deftest rassoc-5
    (rassoc 'a '((b . a)))
  (b . a))

(deftest rassoc-6
    (rassoc 'a (rev-assoc-list '((:a . b) (#:a . c) (a . d) (a . e) (z . f))))
  (d . a))

(deftest rassoc-7
    (let* ((x (copy-tree (rev-assoc-list '((a . b) (b . c) (c . d)))))
	   (xcopy (make-scaffold-copy x))
	   (result (rassoc 'b x)))
      (and
       (eqt result (second x))
       (check-scaffold-copy x xcopy)))
  t)

(deftest rassoc-8
    (rassoc 1 (rev-assoc-list '((0 . a) (1 . b) (2 . c))))
  (b . 1))

(deftest rassoc-9
    (rassoc (copy-seq "abc")
	   (rev-assoc-list '((abc . 1) ("abc" . 2) ("abc" . 3))))
  nil)

(deftest rassoc-10
    (rassoc (copy-list '(a))
	    (copy-tree (rev-assoc-list '(((a) b) ((a) (c))))))
  nil)

(deftest rassoc-11
    (let ((x (list 'a 'b)))
      (rassoc x
	       (rev-assoc-list `(((a b) c) (,x . d) (,x . e) ((a b) 1)))))
  (d a b))


(deftest rassoc-12
    (rassoc #\e
	    (copy-tree
	     (rev-assoc-list '(("abefd" . 1) ("aevgd" . 2) ("edada" . 3))))
	   :key #'(lambda (x) (char x 1)))
  (2 . "aevgd"))

(deftest rassoc-13
    (rassoc nil
	    (copy-tree
	     (rev-assoc-list
	      '(((a) . b) ( nil . c ) ((nil) . d))))
	   :key #'car)
  (c))

(deftest rassoc-14
    (rassoc (copy-seq "abc")
	    (copy-tree
	     (rev-assoc-list
	      '((abc . 1) ("abc" . 2) ("abc" . 3))))
	   :test #'equal)
  (2 . "abc"))

(deftest rassoc-15
    (rassoc (copy-seq "abc")
	    (copy-tree
	     (rev-assoc-list
	      '((abc . 1) ("abc" . 2) ("abc" . 3))))
	   :test #'equalp)
  (2 . "abc"))

(deftest rassoc-16
    (rassoc (copy-list '(a))
	    (copy-tree
	     (rev-assoc-list '(((a) b) ((a) (c)))))
	   :test #'equal)
  ((b) a))

(deftest rassoc-17
    (rassoc (copy-seq "abc")
	    (copy-tree
	     (rev-assoc-list
	      '((abc . 1) (a . a) (b . b) ("abc" . 2) ("abc" . 3))))
	   :test-not (complement #'equalp))
  (2 . "abc"))

(deftest rassoc-18
    (rassoc 'a 
	    (copy-tree
	     (rev-assoc-list
	      '((a . d)(b . c))))
	    :test-not #'eq)
  (c . b))
     
(deftest rassoc-19
    (rassoc 'a
	    (copy-tree
	     (rev-assoc-list
	      '((a . d)(b . c))))
	    :test (complement #'eq))
  (c . b))

(deftest rassoc-20
    (rassoc "a"
	    (copy-tree
	     (rev-assoc-list
	      '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))))
	   :key #'(lambda (x) (and (stringp x) (string-downcase x)))
	   :test #'equal)
  (6 . "A"))

(deftest rassoc-21
    (rassoc "a"
	    (copy-tree
	     (rev-assoc-list
	      '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))))
	   :key #'(lambda (x) (and (stringp x) x))
	   :test #'equal)
  (3 . "a"))

(deftest rassoc-22
    (rassoc "a"
	    (copy-tree
	     (rev-assoc-list
	      '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))))
	   :key #'(lambda (x) (and (stringp x) (string-downcase x)))
	   :test-not (complement #'equal))
  (6 . "A"))

(deftest rassoc-23
    (rassoc "a"
	    (copy-tree
	     (rev-assoc-list
	      '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))))
	   :key #'(lambda (x) (and (stringp x) x))
	   :test-not (complement #'equal))
  (3 . "a"))

;; Check that it works when test returns a true value
;; other than T

(deftest rassoc-24
    (rassoc 'a
	    (copy-tree
	     (rev-assoc-list
	      '((b . 1) (a . 2) (c . 3))))
	   :test #'(lambda (x y) (and (eqt x y) 'matched)))
  (2 . a))

;; Check that the order of the arguments to :test is correct

(deftest rassoc-25
    (block fail
      (rassoc 'a '((1 . b) (2 . c) (3 . a))
	     :test #'(lambda (x y)
		       (unless (eqt x 'a) (return-from fail 'fail))
		       (eqt x y))))
  (3 . A))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rassoc-if

(deftest rassoc-if-1
    (let* ((x (rev-assoc-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (rassoc-if #'evenp x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt result (third x))
       result))
  (c . 6))

(deftest rassoc-if-2
    (handler-case
	(let* ((x (rev-assoc-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	       (xcopy (make-scaffold-copy x))
	       (result (rassoc-if #'oddp x :key #'1+)))
	  (and
	   (check-scaffold-copy x xcopy)
	   (eqt result (third x))
	   result))
      (program-error (c) c))
  (c . 6))

(deftest rassoc-if-3
    (let* ((x (rev-assoc-list '((1 . a) nil (3 . b) (6 . c) (7 . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (rassoc-if #'evenp x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt result (fourth x))
       result))
  (c . 6))

(deftest rassoc-if-4
    (rassoc-if #'null
	       (rev-assoc-list '((a . b) nil (c . d) (nil . e) (f . g))))
  (e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rassoc-if-not

(deftest rassoc-if-not-1
    (let* ((x (rev-assoc-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (rassoc-if-not #'oddp x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt result (third x))
       result))
  (c . 6))

(deftest rassoc-if-not-2
    (handler-case
	(let* ((x (rev-assoc-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	       (xcopy (make-scaffold-copy x))
	       (result (rassoc-if-not #'evenp x :key #'1+)))
	  (and
	   (check-scaffold-copy x xcopy)
	   (eqt result (third x))
	   result))
      (program-error (c) c))
  (c . 6))

(deftest rassoc-if-not-3
    (let* ((x (rev-assoc-list '((1 . a) nil (3 . b) (6 . c) (7 . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (rassoc-if-not #'oddp x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt result (fourth x))
       result))
  (c . 6))

(deftest rassoc-if-not-4
    (rassoc-if-not #'identity 
		   (rev-assoc-list '((a . b) nil (c . d) (nil . e) (f . g))))
  (e))

