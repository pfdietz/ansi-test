;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:37:56 1998
;;;; Contains: Testing of CL Features related to "CONS", part 11

(in-package :cl-test)
(use-package :rt)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ldiff, tailp

(deftest ldiff-1
    (let* ((x (copy-tree '(a b c d e f)))
	   (xcopy (make-scaffold-copy x)))
      (let ((result (ldiff x (cdddr x))))
	(and (check-scaffold-copy x xcopy)
	     result)))
  (a b c))

(deftest ldiff-2
  (handler-case
    (let* ((x (copy-tree '(a b c d e f)))
	   (xcopy (make-scaffold-copy x)))
      (let ((result (ldiff x 'a)))
	(and
	 (check-scaffold-copy x xcopy)
	 (zerop
	  (loop
	      for a on x and b on result count
		(eq a b)))
	 result)))
    (error (c) c))
  (a b c d e f))

;; Works when the end of the dotted list is a symbol
(deftest ldiff-3
    (let* ((x (copy-tree '(a b c d e . f)))
	   (xcopy (make-scaffold-copy x)))
      (let ((result (catch-type-error (ldiff x 'a))))
	(and
	 (check-scaffold-copy x xcopy)
	 result)))
  (a b c d e . f))

;; Works when the end of the dotted list is a fixnum
(deftest ldiff-4
  (let* ((n 18)
	 (x (list* 'a 'b 'c 18))
	 (xcopy (make-scaffold-copy x)))
    (let ((result (catch-type-error (ldiff x n))))
      (and
       (check-scaffold-copy x xcopy)
       result)))
  (a b c))

;; Works when the end of the dotted list is a larger
;; integer (that is eql, but probably not eq).
(deftest ldiff-5
  (let* ((n 18000000000000)
	 (x (list* 'a 'b 'c (1- 18000000000001)))
	 (xcopy (make-scaffold-copy x)))
    (let ((result (catch-type-error (ldiff x n))))
      (and
       (check-scaffold-copy x xcopy)
       result)))
  (a b c))

;; Test works when the end of a dotted list is a string
(deftest ldiff-6
  (let* ((n (copy-seq "abcde"))
	 (x (list* 'a 'b 'c n))
	 (xcopy (make-scaffold-copy x)))
    (let ((result (catch-type-error (ldiff x n))))
      (if (equal result (list 'a 'b 'c))
	  (check-scaffold-copy x xcopy)
	result)))
  t)

;; Check that having the cdr of a dotted list be string-equal, but
;; not eql, does not result in success
(deftest ldiff-6b
  (let* ((n (copy-seq "abcde"))
	 (x (list* 'a 'b 'c n))
	 (xcopy (make-scaffold-copy x)))
    (let ((result (catch-type-error (ldiff x (copy-seq n)))))
      (if (equal result x)
	  (check-scaffold-copy x xcopy)
	result)))
  t)

;; Check that on failure, the list returned by ldiff is
;; a copy of the list, not the list itself.

(deftest ldiff-6a
    (let ((x (list 'a 'b 'c 'd)))
      (let ((result (ldiff x '(e))))
	(and (equal x result)
	     (loop
		 for c1 on x
		 for c2 on result
		 count (eq c1 c2)))))
  0)
	       

;; Error checking

(deftest ldiff-7
    (catch-type-error (ldiff 10 'a))
  type-error)

;; Single atoms are not dotted lists, so the next
;; case should be a type-error
(deftest ldiff-8
    (catch-type-error (ldiff 'a 'a))
  type-error)

(deftest ldiff-9
    (catch-type-error (ldiff (make-array '(10) :initial-element 'a) '(a)))
  type-error)

(deftest ldiff-10
    (catch-type-error (ldiff 1.23 t))
  type-error)

(deftest ldiff-11
    (catch-type-error (ldiff #\w 'a))
  type-error)

;; Note!  The spec is ambiguous on whether this next test
;; is correct.  The spec says that ldiff should be prepared
;; to signal an error if the list argument is not a proper
;; list or dotted list.  If listp is false, the list argument
;; is neither (atoms are not dotted lists).
;;
;; However, the sample implementation *does* work even if
;; the list argument is an atom.
;;
#|
(defun ldiff-12-body ()
    (loop
	for x in *universe*
	count (and (not (listp x))
		   (not (eq 'type-error
			    (catch-type-error (ldiff x x)))))))

(deftest ldiff-12
    (ldiff-12-body)
  0)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tailp

(deftest tailp-1
    (let ((x (copy-tree '(a b c d e . f))))
      (and
       (tailp x x)
       (tailp (cdr x) x)
       (tailp (cddr x) x)
       (tailp (cdddr x) x)
       (tailp (cddddr x) x)
       t))
  t)


;; The next four tests test that tailp handles dotted lists.  See
;; TAILP-NIL:T in the X3J13 documentation.

(deftest tailp-2
    (catch-type-error (not (not (tailp 'e (copy-tree '(a b c d . e))))))
  t)

(deftest tailp-3
    (catch-type-error (tailp 'z (copy-tree '(a b c d . e))))
  nil)

(deftest tailp-4
  (catch-type-error (not (not (tailp 10203040506070
	(list* 'a 'b (1- 10203040506071))))))
  t)

(deftest tailp-5
  (let ((x "abcde"))
    (catch-type-error (tailp x
	  (list* 'a 'b (copy-seq x)))))
  nil)

;; Test that tailp does not modify its arguments

(deftest tailp-6
    (let* ((x (copy-list '(a b c d e)))
	   (y (cddr x)))
      (let ((xcopy (make-scaffold-copy x))
	    (ycopy (make-scaffold-copy y)))
	(and
	 (tailp y x)
	 (check-scaffold-copy x xcopy)
	 (check-scaffold-copy y ycopy))))
  t)

;; Error checking

;; Note!  The spec is ambiguous on whether this next test
;; is correct.  The spec says that tailp should be prepared
;; to signal an error if the list argument is not a proper
;; list or dotted list.  If listp is false, the list argument
;; is neither (atoms are not dotted lists).
;;
;; However, the sample implementation *does* work even if
;; the list argument is an atom.
;;

#|
(defun tailp-7-body ()
  (loop
      for x in *universe*
      count (and (not (listp x))
		 (eq 'type-error
		     (catch-type-error (tailp x x))))))

(deftest tailp-7
    (tailp-7-body)
  0)
|#
    