;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:32:20 1998
;;;; Contains: Testing of CL Features related to "CONS", part 3

(in-package :cl-test)

(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; copy-list

(deftest copy-list.1
  (check-copy-list '(a b c d))
  (a b c d))

;; Check that copy-list works on dotted lists

(deftest copy-list.2
  (check-copy-list '(a . b))
 (a . b))

(deftest copy-list.3
  (check-copy-list '(a b c . d))
  (a b c . d))

(deftest copy-list.4
  (let ((i 0))
    (values (copy-list (progn (incf i) '(a b c)))
	    i))
  (a b c) 1)

(deftest copy-list.error.1
  (classify-error (copy-list))
  program-error)

(deftest copy-list.error.2
  (classify-error (copy-list nil nil))
  program-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list, list*

(deftest list.1
  (list 'a 'b 'c)
  (a b c))

(deftest list.2
  (list)
  nil)

(deftest list.order.1
  (let ((i 0))
    (list (incf i) (incf i) (incf i) (incf i)))
  (1 2 3 4))

(deftest list.order.2
  (let ((i 0))
    (list (incf i) (incf i) (incf i) (incf i)
	  (incf i) (incf i) (incf i) (incf i)))
  (1 2 3 4 5 6 7 8))

(deftest list.order.3
  (let ((i 0))
    (list (incf i) (incf i) (incf i) (incf i)
	  (incf i) (incf i) (incf i) (incf i)
	  (incf i) (incf i) (incf i) (incf i)
	  (incf i) (incf i) (incf i) (incf i)))
  (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))

(deftest list*.1
  (list* 1 2 3)
  (1 2 . 3))

(deftest list*.2
  (list* 'a)
  a)

(deftest list-list*.1
  (list* 'a 'b 'c (list 'd 'e 'f))
  (a b c d e f))

(deftest list*.3
  (list* 1)
  1)

(deftest list*.order.1
  (let ((i 0))
    (list* (incf i) (incf i) (incf i) (incf i)))
  (1 2 3 . 4))

(deftest list*.order.2
  (let ((i 0))
    (list* (incf i) (incf i) (incf i) (incf i)
	   (incf i) (incf i) (incf i) (incf i)
	   (incf i) (incf i) (incf i) (incf i)
	   (incf i) (incf i) (incf i) (incf i)))
  (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 . 16))

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

(deftest list-length.order.1
  (let ((i 0))
    (values (list-length (progn (incf i) '(a b c))) i))
  3 1)


;; Check that list-length produces a type-error
;; on arguments that are not proper lists or circular lists

(deftest list-length.error.1
  (loop
   for x in (list 'a 1 1.0 #\w (make-array '(10))
		  '(a b . c) (symbol-package 'cons))
   count (not (eqt (catch-type-error (list-length x))
		   'type-error)))
  0)

(deftest list-length.error.2
  (classify-error (list-length))
  program-error)

(deftest list-length.error.3
  (classify-error (list-length nil nil))
  program-error)

(deftest list-length.error.4
  (classify-error (list-length 'a))
  type-error)

(deftest list-length.error.5
  (classify-error (locally (list-length 'a) t))
  type-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; listp

;; Check listp against various simple cases

(deftest listp-nil
  (notnot-mv (listp nil))
  t)

(deftest listp-symbol
  (listp 'a)
  nil)

(deftest listp-singleton-list
  (notnot-mv (listp '(a)))
  t)

(deftest listp-circular-list
  (let ((x (cons nil nil)))
    (setf (cdr x) x)
    (notnot-mv (listp x)))
  t)

(deftest listp-longer-list
  (notnot-mv (listp '(a b c d e f g h)))
  t)

;;; Check that (listp x) == (typep x 'list)

(deftest listp-universe
  (check-type-predicate 'listp 'list)
  0)

(deftest listp.order.1
  (let ((i 0))
    (values (listp (incf i)) i))
  nil 1)

(deftest listp.error.1
  (classify-error (listp))
  program-error)

(deftest listp.error.2
  (classify-error (listp nil nil))
  program-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (typep <obj> 'list)

;;; These tests are now somewhat redundant

(deftest typep-nil-list
  (notnot-mv (typep nil 'list))
  t)

(deftest typep-symbol-list
  (typep 'a 'list)
  nil)

(deftest typep-singleton-list-list
  (notnot-mv (typep '(a) 'list))
  t)

(deftest typep-circular-list-list
  (let ((x (cons nil nil)))
    (setf (cdr x) x)
    (notnot-mv (typep x 'list)))
  t)

(deftest typep-longer-list-list
  (notnot-mv (typep '(a b c d e f g h) 'list))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make-list

(deftest make-list-empty.1
  (make-list 0)
  nil)

(deftest make-list-empty.2
  (make-list 0 :initial-element 'a)
  nil)

(deftest make-list-no-initial-element
  (make-list 6)
  (nil nil nil nil nil nil))

(deftest make-list-with-initial-element
  (make-list 6 :initial-element 'a)
  (a a a a a a))

(deftest make-list.allow-other-keys.1
  (make-list 5 :allow-other-keys t :foo 'a)
  (nil nil nil nil nil))

(deftest make-list.allow-other-keys.2
  (make-list 5 :bar nil :allow-other-keys t)
  (nil nil nil nil nil))

(deftest make-list.allow-other-keys.3
  (make-list 5 :allow-other-keys nil)
  (nil nil nil nil nil))

(deftest make-list.allow-other-keys.4
  (make-list 5 :allow-other-keys t :allow-other-keys nil 'bad t)
  (nil nil nil nil nil))

(deftest make-list.allow-other-keys.5
  (make-list 5 :allow-other-keys t)
  (nil nil nil nil nil))

(deftest make-list-repeated-keyword
  (make-list 5 :initial-element 'a :initial-element 'b)
  (a a a a a))

(deftest make-list.order.1
  (let ((i 0) x y)
    (values
     (make-list (progn (setf x (incf i)) 5)
		:initial-element
		(progn (setf y (incf i)) 'a))
     i x y))
  (a a a a a)
  2 1 2)

(deftest make-list.order.2
  (let ((i 0) x y z)
    (values
     (make-list (progn (setf x (incf i)) 5)
		:initial-element
		(progn (setf y (incf i)) 'a)
		:initial-element
		(progn (setf z (incf i)) 'b))
     i x y z))
  (a a a a a)
  3 1 2 3)


(deftest make-list.error.1
  (catch-type-error (make-list -1))
  type-error)

(deftest make-list.error.2
  (classify-error (make-list 'a))
  type-error)

(deftest make-list.error.3
  (classify-error (make-list))
  program-error)

(deftest make-list.error.4
  (classify-error (make-list 5 :bad t))
  program-error)

(deftest make-list.error.5
  (classify-error (make-list 5 :initial-element))
  program-error)

(deftest make-list.error.6
  (classify-error (make-list 5 1 2))
  program-error)

(deftest make-list.error.7
  (classify-error (make-list 5 :bad t :allow-other-keys nil))
  program-error)

(deftest make-list.error.8
  (classify-error (locally (make-list 'a) t))
  type-error)
