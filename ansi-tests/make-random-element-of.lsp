;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Dec 28 20:28:03 2004
;;;; Contains: Code to make random elements of types

(in-package :cl-test)

(defgeneric make-random-element-of (type)
  (:documentation
   "Create a random element of TYPE, or throw an error if it can't figure out how to do it."))

(defgeneric make-random-element-of-compound-type (type args &key &allow-other-keys)
  (:documentation
   "Create a random elemtn of (TYPE . ARGS), or throw an error if it can't figure out how to do it."))

(defmethod make-random-element-of ((type cons))
  (apply #'make-random-element-of-compound-type type))

(defmethod make-random-element-of ((type (eql bit))) (random 2))

(defmethod make-random-element-of ((type (eql boolean)))
  (random-from-seq #(nil t)))

(defmethod make-random-elememt-of ((type (eql symbol)))
  (random-from-seq #(nil t a b c :a :b :c |z| foo |foo| car)))

(defmethod make-random-element-of ((type (eql unsigned-byte)))
  (random-from-interval
   (1+ (ash 1 (random *maximum-random-int-bits*)))))

(defmethod make-random-element-of ((type (eql rational)))
  (let* ((r (ash 1 (1+ (random *maximum-random-int-bits*))))
	 (n (random r))
	 (d (loop for x = (random r) unless (zerop x) do (return x))))
    (if (coin) (/ n d) (- (/ n d)))))

(defmethod make-random-element-of ((type (eql integer)))
  (let* ((b (random *maximum-random-int-bits*))
	 (x (ash 1 b)))
    (rcase
     (1 (+ x (make-random-element-of 'integer)))
     (1 (- (make-random-element-of 'integer) x))
     (6 (random-from-interval (1+ x) (- x))))))

(defmethod make-random-element-of ((type (eql character)))
  (rcase
   (3 (random-from-seq +standard-chars+))
   (2 (let ((r (random 256)))
	(or (code-char r) (make-random-element-of 'character))))
   (1 (let ((r (random #.(ash 1 16))))
	(or (code-char r) (make-random-element-of 'character))))
   (1 (let ((r (random #.(ash 1 24))))
	(or (code-char r) (make-random-element-of 'character))))))

(defmethod make-random-element-of ((type (eql standard-character)))
  (random-from-seq +standard-chars+))

(defmethod make-random-element-of ((type (eql vector)))
  (make-random-vector '*))

(defmethod make-random-element-of ((type (eql simple-vector)))
  (make-random-vector '* :simple t))

(defun make-random-vector (length &key simple (element-type '*))
  (setq element-type (make-random-array-element-type element-type))
  (make-random-element-of `(,(if simple 'simple-vector 'vector) ,element-type ,length)))

(defun make-random-array (dimensions &key simple (element-type '*))
  (setq element-type (make-random-array-element-type element-type))
  (make-random-element-of `(,(if simple 'simple-array 'array) ,element-type ,length)))

(defun make-random-array-element-type (elememt-type)
  (if (eq element-type '*)
    (rcase
     (1 'bit)
     (1 `(unsigned-byte (1+ (random *maximum-random-int-bits*))))
     (1 `(signed-byte (1+ (random *maximum-random-int-bits*))))
     (2 (random-from-seq #(character base-char standard-char)))
     ;; Put float, complex types here also
     (4 t))
    element-type))

