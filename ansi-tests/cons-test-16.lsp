;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:41:13 1998
;;;; Contains: Testing of CL Features related to "CONS", part 16

(in-package :cl-test)
(use-package :rt)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; acons

(deftest acons-1
    (let* ((x (copy-tree '((c . d) (e . f))))
	   (xcopy (make-scaffold-copy x))
	   (result (acons 'a 'b x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt (cdr result) x)
       result))
  ((a . b) (c . d) (e . f)))

(deftest acons-2
    (acons 'a 'b nil)
  ((a . b)))

(deftest acons-3
    (acons 'a 'b 'c)
  ((a . b) . c))

(deftest acons-4
    (acons '((a b)) '(((c d) e) f) '((1 . 2)))
  (( ((a b)) . (((c d) e) f)) (1 . 2)))

(deftest acons-5
    (acons "ancd" 1.143 nil)
  (("ancd" . 1.143)))

(deftest acons-6
    (acons #\R :foo :bar)
  ((#\R . :foo) . :bar))

(deftest acons.error.1
  (classify-error (acons))
  program-error)

(deftest acons.error.2
  (classify-error (acons 'a))
  program-error)

(deftest acons.error.3
  (classify-error (acons 'a 'b))
  program-error)

(deftest acons.error.4
  (classify-error (acons 'a 'b 'c 'd))
  program-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; assoc

(deftest assoc-1
    (assoc nil nil)
  nil)

(deftest assoc-2
    (assoc nil '(nil))
  nil)

(deftest assoc-3
    (assoc nil '(nil (nil . 2) (a . b)))
  (nil . 2))

(deftest assoc-4
    (assoc nil '((a . b) (c . d)))
  nil)

(deftest assoc-5
    (assoc 'a '((a . b)))
  (a . b))

(deftest assoc-6
    (assoc 'a '((:a . b) (#:a . c) (a . d) (a . e) (z . f)))
  (a . d))

(deftest assoc-7
    (let* ((x (copy-tree '((a . b) (b . c) (c . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (assoc 'b x)))
      (and
       (eqt result (second x))
       (check-scaffold-copy x xcopy)))
  t)

(deftest assoc-8
    (assoc 1 '((0 . a) (1 . b) (2 . c)))
  (1 . b))

(deftest assoc-9
    (assoc (copy-seq "abc")
	   '((abc . 1) ("abc" . 2) ("abc" . 3)))
  nil)

(deftest assoc-10
    (assoc (copy-list '(a)) (copy-tree '(((a) b) ((a) (c)))))
  nil)

(deftest assoc-11
    (let ((x (list 'a 'b)))
      (assoc x `(((a b) c) (,x . d) (,x . e) ((a b) 1))))
  ((a b) . d))


(deftest assoc-12
    (assoc #\e '(("abefd" . 1) ("aevgd" . 2) ("edada" . 3))
	   :key #'(lambda (x) (char x 1)))
  ("aevgd" . 2))

(deftest assoc-13
    (assoc nil '(((a) . b) ( nil . c ) ((nil) . d))
	   :key #'car)
  (nil . c))

(deftest assoc-14
    (assoc (copy-seq "abc")
	   '((abc . 1) ("abc" . 2) ("abc" . 3))
	   :test #'equal)
  ("abc" . 2))

(deftest assoc-15
    (assoc (copy-seq "abc")
	   '((abc . 1) ("abc" . 2) ("abc" . 3))
	   :test #'equalp)
  ("abc" . 2))

(deftest assoc-16
    (assoc (copy-list '(a)) (copy-tree '(((a) b) ((a) (c))))
	   :test #'equal)
  ((a) b))

(deftest assoc-17
    (assoc (copy-seq "abc")
	   '((abc . 1) (a . a) (b . b) ("abc" . 2) ("abc" . 3))
	   :test-not (complement #'equalp))
  ("abc" . 2))

(deftest assoc-18
    (assoc 'a '((a . d)(b . c)) :test-not #'eq)
  (b . c))
     
(deftest assoc-19
    (assoc 'a '((a . d)(b . c)) :test (complement #'eq))
  (b . c))

(deftest assoc-20
    (assoc "a" '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))
	   :key #'(lambda (x) (and (stringp x) (string-downcase x)))
	   :test #'equal)
  ("A" . 6))

(deftest assoc-21
    (assoc "a" '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))
	   :key #'(lambda (x) (and (stringp x) x))
	   :test #'equal)
  ("a" . 3))

(deftest assoc-22
    (assoc "a" '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))
	   :key #'(lambda (x) (and (stringp x) (string-downcase x)))
	   :test-not (complement #'equal))
  ("A" . 6))

(deftest assoc-23
    (assoc "a" '(("" . 1) (a . 2) ("A" . 6) ("a" . 3) ("A" . 5))
	   :key #'(lambda (x) (and (stringp x) x))
	   :test-not (complement #'equal))
  ("a" . 3))

;; Check that it works when test returns a true value
;; other than T

(deftest assoc-24
    (assoc 'a '((b . 1) (a . 2) (c . 3))
	   :test #'(lambda (x y) (and (eqt x y) 'matched)))
  (a . 2))

;; Check that the order of the arguments to test is correct

(deftest assoc-25
    (block fail
      (assoc 'a '((b . 1) (c . 2) (a . 3))
	     :test #'(lambda (x y)
		       (unless (eqt x 'a) (return-from fail 'fail))
		       (eqt x y))))
  (A . 3))

(deftest assoc-26
  (assoc 'b '((a . 1) (b . 2) (c . 3)) :bad t :allow-other-keys t)
  (b . 2))

(deftest assoc-27
  (assoc 'b '((a . 1) (b . 2) (c . 3)) :allow-other-keys t :also-bad t)
  (b . 2))

(deftest assoc-28
  (assoc 'b '((a . 1) (b . 2) (c . 3)) :allow-other-keys t :also-bad t
	 :test-not #'eql)
  (a . 1))

(deftest assoc.error.1
  (classify-error (assoc))
  program-error)

(deftest assoc.error.2
  (classify-error (assoc nil))
  program-error)

(deftest assoc.error.3
  (classify-error (assoc nil nil :bad t))
  program-error)

(deftest assoc.error.4
  (classify-error (assoc nil nil :key))
  program-error)

(deftest assoc.error.5
  (classify-error (assoc nil nil  1 1))
  program-error)

(deftest assoc.error.6
  (classify-error (assoc nil nil :bad t :allow-other-keys nil))
  program-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; assoc-if

(deftest assoc-if-1
    (let* ((x (copy-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (assoc-if #'evenp x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt result (third x))
       result))
  (6 . c))

(deftest assoc-if-2
  (let* ((x (copy-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	 (xcopy (make-scaffold-copy x))
	 (result (assoc-if #'oddp x :key #'1+)))
    (and
     (check-scaffold-copy x xcopy)
     (eqt result (third x))
     result))
  (6 . c))

(deftest assoc-if-3
    (let* ((x (copy-list '((1 . a) nil (3 . b) (6 . c) (7 . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (assoc-if #'evenp x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt result (fourth x))
       result))
  (6 . c))

(deftest assoc-if-4
    (assoc-if #'null '((a . b) nil (c . d) (nil . e) (f . g)))
  (nil . e))

(deftest assoc-if-5
  (assoc-if #'null '((a . 1) (nil . 2) (c . 3)) :bad t :allow-other-keys t)
  (nil . 2))

(deftest assoc-if-6
  (assoc-if #'null '((a . 1) (nil . 2) (c . 3))
	    :allow-other-keys t :also-bad t)
  (nil . 2))

(deftest assoc-if-7
  (assoc-if #'null '((a . 1) (nil . 2) (c . 3))
	    :allow-other-keys t :also-bad t :key #'not)
  (a . 1))

;;; Error cases

(deftest assoc-if.error.1
  (classify-error (assoc-if))
  program-error)

(deftest assoc-if.error.2
  (classify-error (assoc-if #'null))
  program-error)

(deftest assoc-if.error.3
  (classify-error (assoc-if #'null nil :bad t))
  program-error)

(deftest assoc-if.error.4
  (classify-error (assoc-if #'null nil :key))
  program-error)

(deftest assoc-if.error.5
  (classify-error (assoc-if #'null nil 1 1))
  program-error)

(deftest assoc-if.error.6
  (classify-error (assoc-if #'null nil :bad t :allow-other-keys nil))
  program-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; assoc-if-not

(deftest assoc-if-not-1
    (let* ((x (copy-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (assoc-if-not #'oddp x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt result (third x))
       result))
  (6 . c))

(deftest assoc-if-not-2
  (let* ((x (copy-list '((1 . a) (3 . b) (6 . c) (7 . d))))
	 (xcopy (make-scaffold-copy x))
	 (result (assoc-if-not #'evenp x :key #'1+)))
    (and
     (check-scaffold-copy x xcopy)
     (eqt result (third x))
     result))
  (6 . c))

(deftest assoc-if-not-3
    (let* ((x (copy-list '((1 . a) nil (3 . b) (6 . c) (7 . d))))
	   (xcopy (make-scaffold-copy x))
	   (result (assoc-if-not #'oddp x)))
      (and
       (check-scaffold-copy x xcopy)
       (eqt result (fourth x))
       result))
  (6 . c))

(deftest assoc-if-not-4
    (assoc-if-not #'identity '((a . b) nil (c . d) (nil . e) (f . g)))
  (nil . e))

(deftest assoc-if-not-5
  (assoc-if-not #'identity
		'((a . 1) (nil . 2) (c . 3)) :bad t :allow-other-keys t)
  (nil . 2))

(deftest assoc-if-not-6
  (assoc-if-not #'identity '((a . 1) (nil . 2) (c . 3))
	    :allow-other-keys t :also-bad t)
  (nil . 2))

(deftest assoc-if-not-7
  (assoc-if-not #'identity '((a . 1) (nil . 2) (c . 3))
	    :allow-other-keys t :also-bad t :key #'not)
  (a . 1))

(deftest assoc-if-not.error.1
  (classify-error (assoc-if-not))
  program-error)

(deftest assoc-if-not.error.2
  (classify-error (assoc-if-not #'null))
  program-error)

(deftest assoc-if-not.error.3
  (classify-error (assoc-if-not #'null nil :bad t))
  program-error)

(deftest assoc-if-not.error.4
  (classify-error (assoc-if-not #'null nil :key))
  program-error)

(deftest assoc-if-not.error.5
  (classify-error (assoc-if-not #'null nil 1 1))
  program-error)

(deftest assoc-if-not.error.6
  (classify-error (assoc-if-not #'null nil :bad t :allow-other-keys nil))
  program-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; copy-alist

(deftest copy-alist-1
    (let* ((x (copy-tree '((a . b) (c . d) nil (e f) ((x) ((y z)) w)
			   ("foo" . "bar") (#\w . 1.234)
			   (1/3 . 4123.4d5))))
	   (xcopy (make-scaffold-copy x))
	   (result (copy-alist x)))
      (and
       (check-scaffold-copy x xcopy)
       (= (length x) (length result))
       (every #'(lambda (p1 p2)
		  (or (and (null p1) (null p2))
		      (and (not (eqt p1 p2))
			   (eqt (car p1) (car p2))
			   (eqt (cdr p1) (cdr p2)))))
	      x result)
       t))
  t)

(deftest copy-alist.error.1
  (classify-error (copy-alist))
  program-error)

(deftest copy-alist.error.2
  (classify-error (copy-alist nil nil))
  program-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pairlis

;; Pairlis has two legal behaviors: the pairs
;; can be prepended in the same order, or in the
;; reverse order, that they appear in the first
;; two arguments

(defun my-pairlis (x y &optional alist)
  (if (null x)
      alist
    (acons (car x) (car y)
	   (my-pairlis (cdr x) (cdr y) alist))))

(deftest pairlis-1
    (pairlis nil nil nil)
  nil)

(deftest pairlis-2
    (pairlis '(a) '(b) nil)
  ((a . b)))

(deftest pairlis-3
    (let* ((x (copy-list '(a b c d e)))
	   (xcopy (make-scaffold-copy x))
	   (y (copy-list '(1 2 3 4 5)))
	   (ycopy (make-scaffold-copy y))
	   (result (pairlis x y))
	   (expected (my-pairlis x y)))
      (and
       (check-scaffold-copy x xcopy)
       (check-scaffold-copy y ycopy)
       (or
	(equal result expected)
	(equal result (reverse expected)))
       t))
  t)

(deftest pairlis-4
    (let* ((x (copy-list '(a b c d e)))
	   (xcopy (make-scaffold-copy x))
	   (y (copy-list '(1 2 3 4 5)))
	   (ycopy (make-scaffold-copy y))
	   (z '((x . 10) (y . 20)))
	   (zcopy (make-scaffold-copy z))
	   (result (pairlis x y z))
	   (expected (my-pairlis x y z)))
      (and
       (check-scaffold-copy x xcopy)
       (check-scaffold-copy y ycopy)
       (check-scaffold-copy z zcopy)
       (eqt (cdr (cddr (cddr result))) z)
       (or
	(equal result expected)
	(equal result (append (reverse (subseq expected 0 5))
			      (subseq expected 5))))
       t))
  t)

(deftest pairlis.error.1
  (classify-error (pairlis))
  program-error)

(deftest pairlis.error.2
  (classify-error (pairlis nil))
  program-error)

(deftest pairlis.error.3
  (classify-error (pairlis nil nil nil nil))
  program-error)
