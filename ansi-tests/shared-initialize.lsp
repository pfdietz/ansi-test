;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Apr 29 04:09:06 2003
;;;; Contains: Tests of SHARED-INITIALIZE

(in-package :cl-test)

(defclass shared-init-class-01 ()
  ((a :initform 'x :initarg :a)
   (b :initform 'y :initarg :b)
   (c :initarg :c)
   d))

(deftest shared-initialize.1.1
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil :a 1 :b 3 :c 14))
     (map-slot-boundp* obj '(a b c d))
     (map-slot-value obj '(a b c))))
  (nil nil nil nil)
  t
  (t t t nil)
  (1 3 14))

(deftest shared-initialize.1.2
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil))
     (map-slot-boundp* obj '(a b c d))))
  (nil nil nil nil)
  t
  (nil nil nil nil))

(deftest shared-initialize.1.3
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil :a 1 :a 2))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (nil nil nil nil)
  t
  (t nil nil nil)
  1)

(deftest shared-initialize.1.4
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil :a 1 :a 2 :allow-other-keys nil))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (nil nil nil nil)
  t
  (t nil nil nil)
  1)

(deftest shared-initialize.1.5
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj '(a) :a 1))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (nil nil nil nil)
  t
  (t nil nil nil)
  1)

(deftest shared-initialize.1.6
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj '(a)))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (nil nil nil nil)
  t
  (t nil nil nil)
  x)

(deftest shared-initialize.1.7
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj t))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)
     (slot-value obj 'b)))
  (nil nil nil nil)
  t
  (t t nil nil)
  x y)

(deftest shared-initialize.1.8
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj t :b 10 :c 100))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)
     (slot-value obj 'b)
     (slot-value obj 'c)))
  (nil nil nil nil)
  t
  (t t t nil)
  x 10 100)

(deftest shared-initialize.1.9
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj nil :a 1 :b 10 :c 100))
     (eqt obj (shared-initialize obj nil :a 5 :b 37 :c 213))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)
     (slot-value obj 'b)
     (slot-value obj 'c)))
  (nil nil nil nil)
  t t
  (t t t nil)
  5 37 213)

(deftest shared-initialize.1.10
  (let ((obj (allocate-instance (find-class 'shared-init-class-01))))
    (setf (slot-value obj 'a) 1000)
    (values
     (map-slot-boundp* obj '(a b c d))
     (eqt obj (shared-initialize obj '(a)))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  (t nil nil nil)
  t
  (t nil nil nil)
  1000)

;;; Initforms in the lexical environment of the defclass

(declaim (special *shared-init-var-02-init*
		  *shared-init-var-02-query*))

(let ((ainit 0) (binit 0))
  (flet ((%init (a b) (setf ainit a binit b))
	 (%query () (list ainit binit)))
    (setf *shared-init-var-02-init* #'%init
	  *shared-init-var-02-query* #'%query)
    (defclass shared-init-class-02 ()
      ((a :initform (incf ainit) :initarg :a)
       (b :initform (incf binit) :initarg :b)
       (c :initarg :c)
       (d))
      (:default-initargs :c 100))))

(deftest shared-initialize.2.1
  (progn
    (funcall *shared-init-var-02-init* 5 10)
    (let ((obj (allocate-instance (find-class 'shared-init-class-02))))
      (values
       (funcall *shared-init-var-02-query*)
       (eqt obj (shared-initialize obj t))
       (slot-value obj 'a)
       (slot-value obj 'b)
       (map-slot-boundp* obj '(a b c d))
       (funcall *shared-init-var-02-query*))))
  (5 10)
  t
  6 11
  (t t nil nil)
  (6 11))

(deftest shared-initialize.2.2
  (progn
    (funcall *shared-init-var-02-init* 5 10)
    (let ((obj (allocate-instance (find-class 'shared-init-class-02))))
      (values
       (funcall *shared-init-var-02-query*)
       (eqt obj (shared-initialize obj nil))
       (map-slot-boundp* obj '(a b c d))
       (funcall *shared-init-var-02-query*))))
  (5 10)
  t
  (nil nil nil nil)
  (5 10))

(deftest shared-initialize.2.3
  (progn
    (funcall *shared-init-var-02-init* 5 10)
    (let ((obj (allocate-instance (find-class 'shared-init-class-02))))
      (values
       (funcall *shared-init-var-02-query*)
       (eqt obj (shared-initialize obj '(a)))
       (slot-value obj 'a)
       (map-slot-boundp* obj '(a b c d))
       (funcall *shared-init-var-02-query*))))
  (5 10)
  t
  6
  (t nil nil nil)
  (6 10))

(deftest shared-initialize.2.4
  (progn
    (funcall *shared-init-var-02-init* 5 10)
    (let ((obj (allocate-instance (find-class 'shared-init-class-02))))
      (values
       (funcall *shared-init-var-02-query*)
       (eqt obj (shared-initialize obj '(b)))
       (slot-value obj 'b)
       (map-slot-boundp* obj '(a b c d))
       (funcall *shared-init-var-02-query*))))
  (5 10)
  t
  11
  (nil t nil nil)
  (5 11))

(deftest shared-initialize.2.5
  (progn
    (funcall *shared-init-var-02-init* 5 10)
    (let ((obj (allocate-instance (find-class 'shared-init-class-02))))
      (values
       (funcall *shared-init-var-02-query*)
       (eqt obj (shared-initialize obj t :a 34 :b 49))
       (map-slot-value obj '(a b))
       (map-slot-boundp* obj '(a b c d))
       (funcall *shared-init-var-02-query*))))
  (5 10)
  t
  (34 49)
  (t t nil nil)
  (5 10))

(deftest shared-initialize.2.6
  (progn
    (funcall *shared-init-var-02-init* 5 10)
    (let ((obj (allocate-instance (find-class 'shared-init-class-02))))
      (values
       (funcall *shared-init-var-02-query*)
       (eqt obj (shared-initialize obj '(a b c d) :a 34 :b 49))
       (map-slot-value obj '(a b))
       (map-slot-boundp* obj '(a b c d))
       (funcall *shared-init-var-02-query*))))
  (5 10)
  t
  (34 49)
  (t t nil nil)
  (5 10))

;;; Defining new methods on shared-initialize

(defstruct shared-init-class-03
  a b c)

(defmethod shared-initialize ((obj shared-init-class-03)
			      slots-to-init
			      &key
			      (a nil a-p)
			      (b nil b-p)
			      (c nil c-p)
			      &allow-other-keys)
  (declare (ignore slots-to-init))
  (when a-p (setf (slot-value obj 'a) a))
  (when b-p (setf (slot-value obj 'b) b))
  (when c-p (setf (slot-value obj 'c) c))
  obj)

(deftest shared-initialize.3.1
  (let ((obj (make-shared-init-class-03)))
    (values
     (eqt obj (shared-initialize obj nil :a 1 :b 5 :c 19))
     (shared-init-class-03-a obj)
     (shared-init-class-03-b obj)
     (shared-init-class-03-c obj)))
  t 1 5 19)


;;; Inheritance

(defclass shared-init-class-04a ()
  ((a :initform 4 :initarg :a)
   (b :initform 8 :initarg :b)))

(defclass shared-init-class-04b (shared-init-class-04a)
  ((c :initform 17 :initarg :c) d)
  (:default-initargs :a 1))

(deftest shared-initialize.4.1
  (let ((obj (allocate-instance (find-class 'shared-init-class-04b))))
    (values
     (eqt obj (shared-initialize obj nil :a 'x))
     (map-slot-boundp* obj '(a b c d))
     (slot-value obj 'a)))
  t
  (t nil nil nil)
  x)

(deftest shared-initialize.4.2
  (let ((obj (allocate-instance (find-class 'shared-init-class-04b))))
    (values
     (eqt obj (shared-initialize obj nil))
     (map-slot-boundp* obj '(a b c d))))
  t
  (nil nil nil nil))

(deftest shared-initialize.4.3
  (let ((obj (allocate-instance (find-class 'shared-init-class-04b))))
    (values
     (eqt obj (shared-initialize obj t))
     (map-slot-boundp* obj '(a b c d))
     (map-slot-value obj '(a b c))))
  t
  (t t t nil)
  (4 8 17))

(deftest shared-initialize.4.4
  (let ((obj (allocate-instance (find-class 'shared-init-class-04b))))
    (values
     (eqt obj (shared-initialize obj '(a c)))
     (map-slot-boundp* obj '(a b c d))
     (map-slot-value obj '(a c))))
  t
  (t nil t nil)
  (4 17))

(deftest shared-initialize.4.5
  (let ((obj (allocate-instance (find-class 'shared-init-class-04b))))
    (values
     (eqt obj (shared-initialize obj '(a c) :c 81))
     (map-slot-boundp* obj '(a b c d))
     (map-slot-value obj '(a c))))
  t
  (t nil t nil)
  (4 81))

(deftest shared-initialize.4.6
  (let ((obj (allocate-instance (find-class 'shared-init-class-04b))))
    (values
     (eqt obj (shared-initialize obj '(a c) :a 91))
     (map-slot-boundp* obj '(a b c d))
     (map-slot-value obj '(a c))))
  t
  (t nil t nil)
  (91 17))

