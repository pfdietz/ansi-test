;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 19:44:45 2002
;;;; Contains: Tests on FILL

(in-package :cl-test)

(deftest fill-error.1
  (classify-error (fill 'a 'b))
  type-error)

;;; Fill on arrays

(deftest array-fill-1
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
	 (b (fill a 'x)))
    (values (eqt a b)
	    (map 'list #'identity a)))
  t (x x x x x))

(deftest array-fill-2
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
	 (b (fill a 'x :start 2)))
    (values (eqt a b)
	    (map 'list #'identity a)))
  t (a b x x x))

(deftest array-fill-3
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
	 (b (fill a 'x :end 2)))
    (values (eqt a b)
	    (map 'list #'identity a)))
  t (x x c d e))

(deftest array-fill-4
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
	 (b (fill a 'x :start 1 :end 3)))
    (values (eqt a b)
	    (map 'list #'identity a)))
  t (a x x d e))

(deftest array-fill-5
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
	 (b (fill a 'x :start 1 :end nil)))
    (values (eqt a b)
	    (map 'list #'identity a)))
  t (a x x x x))

(deftest array-fill-6
  (let* ((a (make-array '(5) :initial-contents '(a b c d e)))
	 (b (fill a 'x :end nil)))
    (values (eqt a b)
	    (map 'list #'identity a)))
  t (x x x x x))

(deftest array-fill-7
  (classify-error
   (let* ((a (make-array '(5))))
     (fill a 'x :start -1)))
  type-error)

(deftest array-fill-8
  (classify-error
   (let* ((a (make-array '(5))))
     (fill a 'x :start 'a)))
  type-error)

(deftest array-fill-9
  (classify-error
   (let* ((a (make-array '(5))))
     (fill a 'x :end -1)))
  type-error)

(deftest array-fill-10
  (classify-error
   (let* ((a (make-array '(5))))
     (fill a 'x :end 'a)))
  type-error)

;;; fill on arrays of fixnums

(deftest array-fixnum-fill-1
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
	 (b (fill a 6)))
    (values (eqt a b)
	    (map 'list #'identity a)))
  t (6 6 6 6 6))

(deftest array-fixnum-fill-2
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
	 (b (fill a 6 :start 2)))
    (values (eqt a b)
	    (map 'list #'identity a)))
  t (1 2 6 6 6))

(deftest array-fixnum-fill-3
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
	 (b (fill a 7 :end 2)))
    (values (eqt a b)
	    (map 'list #'identity a)))
  t (7 7 3 4 5))

(deftest array-fixnum-fill-4
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
	 (b (fill a 8 :start 1 :end 3)))
    (values (eqt a b)
	    (map 'list #'identity a)))
  t (1 8 8 4 5))

(deftest array-fixnum-fill-5
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
	 (b (fill a 0 :start 1 :end nil)))
    (values (eqt a b)
	    (map 'list #'identity a)))
  t (1 0 0 0 0))

(deftest array-fixnum-fill-6
  (let* ((a (make-array '(5) :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
	 (b (fill a -1 :end nil)))
    (values (eqt a b)
	    (map 'list #'identity a)))
  t (-1 -1 -1 -1 -1))

(deftest array-fixnum-fill-7
  (classify-error
   (let* ((a (make-array '(5) :element-type 'fixnum)))
     (fill a 10 :start -1)))
  type-error)

(deftest array-fixnum-fill-8
  (classify-error
   (let* ((a (make-array '(5) :element-type 'fixnum)))
     (fill a 100 :start 'a)))
  type-error)

(deftest array-fixnum-fill-9
  (classify-error
   (let* ((a (make-array '(5) :element-type 'fixnum)))
     (fill a -5 :end -1)))
  type-error)

(deftest array-fixnum-fill-10
  (classify-error
   (let* ((a (make-array '(5) :element-type 'fixnum)))
     (fill a 17 :end 'a)))
  type-error)

;;; fill on arrays of unsigned eight bit bytes

(defun array-unsigned-byte-fill-test-fn (byte-size &rest fill-args)
  (let* ((a (make-array '(5) :element-type (list 'unsigned-byte byte-size)
			:initial-contents '(1 2 3 4 5)))
	 (b (apply #'fill a fill-args)))
    (values (eqt a b)
	    (map 'list #'identity a))))

(deftest array-unsigned-byte8-fill-1
  (array-unsigned-byte-fill-test-fn 8 6)
  t (6 6 6 6 6))

(deftest array-unsigned-byte8-fill-2
  (array-unsigned-byte-fill-test-fn 8 6 :start 2)
  t (1 2 6 6 6))

(deftest array-unsigned-byte8-fill-3
  (array-unsigned-byte-fill-test-fn 8 7 :end 2)
  t (7 7 3 4 5))

(deftest array-unsigned-byte8-fill-4
  (array-unsigned-byte-fill-test-fn 8 8 :start 1 :end 3)
  t (1 8 8 4 5))

(deftest array-unsigned-byte8-fill-5
  (array-unsigned-byte-fill-test-fn 8 9 :start 1 :end nil)
  t (1 9 9 9 9))

(deftest array-unsigned-byte8-fill-6
  (array-unsigned-byte-fill-test-fn 8 0 :end nil)
  t (0 0 0 0 0))

(deftest array-unsigned-byte8-fill-7
  (classify-error (array-unsigned-byte-fill-test-fn 8 0 :start -1))
  type-error)

(deftest array-unsigned-byte8-fill-8
  (classify-error (array-unsigned-byte-fill-test-fn 8 100 :start 'a))
  type-error)

(deftest array-unsigned-byte8-fill-9
  (classify-error (array-unsigned-byte-fill-test-fn 8 19 :end -1))
  type-error)

(deftest array-unsigned-byte8-fill-10
  (classify-error (array-unsigned-byte-fill-test-fn 8 17 :end 'a))
  type-error)


