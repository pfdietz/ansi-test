;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:32:20 1998
;;;; Contains: Testing of CL Features related to "CONS", part 3

(in-package :cl-test)
(use-package :rt)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; copy-list

(defun check-copy-list-copy (x y)
  "Check that y is a copy of the list x."
  (if
      (consp x)
      (and (consp y)
	   (not (eq x y))
	   (eq (car x) (car y))
	   (check-copy-list-copy (cdr x) (cdr y)))
    (and (eq x y) t)))

(defun check-copy-list (x)
  "Apply copy-list, checking that it properly copies,
   and checking that it does not change its argument."
  (let ((xcopy (make-scaffold-copy x)))
    (let ((y (copy-list x)))
      (and
       (check-scaffold-copy x xcopy)
       (check-copy-list-copy x y)
       y))))

(deftest copy-list-1
    (check-copy-list '(a b c d))
  (a b c d))

;; Check that copy-list works on dotted lists

(deftest copy-list-2
    (check-copy-list '(a . b))
 (a . b))

(deftest copy-list-3
    (check-copy-list '(a b c . d))
  (a b c . d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list, list*

(deftest list-1
    (list 'a 'b 'c)
  (a b c))

(deftest list-2
    (list)
  nil)

(deftest list*-1
    (list* 1 2 3)
  (1 2 . 3))

(deftest list*-2
    (list* 'a)
  a)

(deftest list-list*-1
    (list* 'a 'b 'c (list 'd 'e 'f))
  (a b c d e f))

(deftest list*-3
    (list* 1)
  1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-length

(deftest list-length-nil
    (list-length nil)
  0)

(deftest list-length-list
    (list-length '(a b c d e f))
  6)


;; check that list-length returns nil
;; on a circular list

(deftest list-length-circular-list
    (let ((x (cons nil nil)))
      (let ((y (list* 1 2 3 4 5 6 7 8 9 x)))
	(setf (cdr x) y)
	(let ((z (list* 'a 'b 'c 'd 'e y)))
	  (list-length z))))
  nil)


;; Check that list-length produces a type-error
;; on arguments that are not proper lists or circular lists

(deftest list-length-error
    (loop
	for x in (list 'a 1 1.0 #\w (make-array '(10))
		       '(a b . c) (symbol-package 'cons))
	count (not (eq (catch-type-error (list-length x))
		       'type-error)))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; listp

;; Check listp against various simple cases

(deftest listp-nil
    (not (not (listp nil)))
  t)

(deftest listp-symbol
    (listp 'a)
  nil)

(deftest listp-singleton-list
    (not (not (listp '(a))))
  t)

(deftest listp-circular-list
    (let ((x (cons nil nil)))
      (setf (cdr x) x)
      (not (not (listp x))))
  t)

(deftest listp-longer-list
    (not (not (listp '(a b c d e f g h))))
  t)

;; Check that (listp x) == (typep x 'list)

(deftest listp-universe
    (check-type-predicate 'listp 'list)
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (typep <obj> 'list)

;; These tests are now somewhat redundant

(deftest typep-nil-list
    (not (not (typep nil 'list)))
  t)

(deftest typep-symbol-list
    (typep 'a 'list)
  nil)

(deftest typep-singleton-list-list
    (not (not (typep '(a) 'list)))
  t)

(deftest typep-circular-list-list
    (let ((x (cons nil nil)))
      (setf (cdr x) x)
      (not (not (typep x 'list))))
  t)

(deftest typep-longer-list-list
    (not (not (typep '(a b c d e f g h) 'list)))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make-list

(deftest make-list-empty-1
    (make-list 0)
  nil)

(deftest make-list-empty-2
    (make-list 0 :initial-element 'a)
  nil)

(deftest make-list-no-initial-element
    (make-list 6)
  (nil nil nil nil nil nil))

(deftest make-list-with-initial-element
    (make-list 6 :initial-element 'a)
  (a a a a a a))

(deftest make-list-type-error-1
    (catch-type-error (make-list -1))
  type-error)

;; This next test causes ACL 4.3 (Linux) to loop
#-allegro
(deftest make-list-type-error-2
    (catch-type-error (make-list 'a))
  type-error)
