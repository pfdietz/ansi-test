;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:34:40 1998
;;;; Contains: Testing of CL Features related to "CONS", part 6

(in-package :cl-test)
(use-package :rt)

(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; endp

(deftest endp-nil
    (not (not (endp nil)))
  t)

(deftest endp-cons
    (endp (cons 'a 'a))
  nil)

(deftest endp-singleton-list
    (endp '(a))
  nil)

(deftest endp-symbol-error
  (catch-type-error (endp 'a))
  type-error)

(deftest endp-fixnum-error
  (catch-type-error (endp 1))
  type-error)

(deftest endp-float-error
  (catch-type-error (endp 0.9212d4))
  type-error)

(deftest endp.error.4
  (classify-error (endp))
  program-error)

(deftest endp.error.5
  (classify-error (endp nil nil))
  program-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; null

(deftest null-1
    (null nil)
  t)

(deftest null-2
    (not (some #'null
	       `(1 a 1.2 "a" #\w (a) ,*terminal-io*
		   #'car (make-array '(10)))))
  t)

(deftest null.error.1
  (classify-error (null))
  program-error)

(deftest null.error.2
  (classify-error (null nil nil))
  program-error)

(deftest not-1
    (not nil)
  t)

(deftest not-2
    (not (some #'not
	       `(1 a 1.2 "a" #\w (a) ,*terminal-io*
		   #'car (make-array '(10)))))
  t)

(deftest not.error.1
  (classify-error (not))
  program-error)

(deftest not.error.2
  (classify-error (not nil nil))
  program-error)

(deftest typep-nil-null
    (not (not (typep nil 'null)))
  t)

(deftest typep-t-null
    (typep t 'null)
  nil)
