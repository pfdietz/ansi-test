;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May  3 14:23:29 2003
;;;; Contains: Tests of CHANGE-CLASS

(in-package :cl-test)

(defclass change-class-class-01a ()
  ((a :initarg :a) (b :initarg :b) (c :initarg :c)))

(defclass change-class-class-01b ()
  ((c :initarg :c2) (d :initarg :d2) (b :initarg :b2)))

(deftest change-class.1.1
  (let ((obj (make-instance 'change-class-class-01a))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (map-slot-boundp* obj '(a b c))
     (slot-exists-p obj 'd)
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))))
  t nil (nil nil nil)
  nil t nil t nil (nil nil nil))

(deftest change-class.1.2
  (let ((obj (make-instance 'change-class-class-01a :a 1))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (map-slot-boundp* obj '(a b c))
     (slot-exists-p obj 'd)
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))))
  t nil (t nil nil)
  nil t nil t nil (nil nil nil))

(deftest change-class.1.3
  (let ((obj (make-instance 'change-class-class-01a :b 2))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (map-slot-boundp* obj '(a b c))
     (slot-exists-p obj 'd)
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (slot-value obj 'b)))
  t nil (nil t nil)
  nil t nil t nil (t nil nil) 2)

(deftest change-class.1.4
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (map-slot-boundp* obj '(a b c))
     (slot-exists-p obj 'd)
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil (t t t)
  nil t nil t nil (t t nil) (2 5))

(deftest change-class.1.5
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (eqt obj (change-class obj new-class :b2 8 :c2 76))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil t nil (t t nil) (8 76))

(deftest change-class.1.6
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (eqt obj (change-class obj new-class :b2 19 :b2 34))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil t nil (t t nil) (19 5))

(deftest change-class.1.7
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (eqt obj (change-class obj new-class :allow-other-keys nil))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil t nil (t t nil) (2 5))

(deftest change-class.1.8
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (eqt obj (change-class obj new-class :allow-other-keys t))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil t nil (t t nil) (2 5))

(deftest change-class.1.9
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (eqt obj (change-class obj new-class :allow-other-keys t
			    :nonsense t))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil t nil (t t nil) (2 5))

(deftest change-class.1.10
  (let ((obj (make-instance 'change-class-class-01a :a 1 :b 2 :c 5))
	(new-class (find-class 'change-class-class-01b)))
    (values
     (eqt obj (change-class obj new-class :bad 0 :allow-other-keys t
			    :allow-other-keys nil :nonsense t))
     (typep* obj 'change-class-class-01a)
     (typep* obj 'change-class-class-01b)
     (slot-exists-p obj 'a)
     (map-slot-boundp* obj '(b c d))
     (map-slot-value obj '(b c))))
  t nil t nil (t t nil) (2 5))

(deftest change-class.1.11
  (handler-case
   (eval
    '(let ((obj (make-instance 'change-class-class-01a))
	   (new-class (find-class 'change-class-class-01b)))
       (eqt obj (change-class obj new-class :nonsense t))))
   (error () :expected-error))
  :expected-error)

;;; Shared slots

(defclass change-class-class-02a ()
  ((a :initarg :a :allocation :class)
   (b :initarg :b :allocation :class)))

(defclass change-class-class-02b ()
  ((a :initarg :a2)
   (b :initarg :b2)))

(deftest change-class.2.1
  (let ((obj (make-instance 'change-class-class-02a))
	(new-class (find-class 'change-class-class-02b)))
    (slot-makunbound obj 'a)
    (slot-makunbound obj 'b)
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-02a)
     (typep* obj 'change-class-class-02b)
     (map-slot-boundp* (make-instance 'change-class-class-02a) '(a b))
     (map-slot-boundp* obj '(a b))))
  (nil nil)
  t nil t
  (nil nil)
  (nil nil))

(deftest change-class.2.2
  (let ((obj (make-instance 'change-class-class-02a))
	(obj2 (make-instance 'change-class-class-02a))
	obj3
	(new-class (find-class 'change-class-class-02b)))
    (setf (slot-value obj 'a) 'foo)
    (slot-makunbound obj 'b)
    (values
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj2 'a)
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-02a)
     (typep* obj 'change-class-class-02b)
     (map-slot-boundp* (setf obj3 (make-instance 'change-class-class-02a))
		       '(a b))
     (map-slot-boundp* obj '(a b))
     (slot-value obj 'a)
     (slot-value obj2 'a)
     (slot-value obj3 'a)
     (eqt obj obj2) (eqt obj obj3) (eqt obj2 obj3)
     ))
  (t nil)
  foo foo
  t nil t
  (t nil)
  (t nil)
  foo foo foo
  nil nil nil)

(deftest change-class.2.3
  (let ((obj (make-instance 'change-class-class-02a))
	(obj2 (make-instance 'change-class-class-02a))
	(new-class (find-class 'change-class-class-02b)))
    (setf (slot-value obj 'a) 1
	  (slot-value obj 'b) 16)
    (values
     (map-slot-boundp* obj '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-02a)
     (typep* obj 'change-class-class-02b)
     (map-slot-boundp* obj2 '(a b))
     (map-slot-boundp* (make-instance 'change-class-class-02a) '(a b))
     (map-slot-boundp* obj '(a b))
     (progn (slot-makunbound obj2 'a)
	    (slot-makunbound obj2 'b)
	    (map-slot-boundp* obj '(a b)))))
		 
  (t t)
  t nil t
  (t t)
  (t t)
  (t t)
  (t t))

;;; Destination slots are shared

(defclass change-class-class-03a ()
  ((a :initarg :a) (b :initarg :b)))

(defclass change-class-class-03b ()
  ((a :allocation :class :initarg :a2)
   (b :allocation :class :initarg :b2)))

(deftest change-class.3.1
  (let* ((obj (make-instance 'change-class-class-03a))
	 (new-class (find-class 'change-class-class-03b))
	 (obj2 (make-instance new-class))
	 obj3)
    (slot-makunbound obj2 'a)
    (slot-makunbound obj2 'b)
    (values
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-03a)
     (typep* obj 'change-class-class-03b)
     (typep* obj new-class)
     (eqt (setq obj3 (make-instance new-class)) obj)
     (map-slot-boundp* obj '(a b))
     (map-slot-boundp* obj2 '(a b))
     (map-slot-boundp* obj3 '(a b))
     ))
  t nil t t nil (nil nil) (nil nil) (nil nil))

(deftest change-class.3.2
  (let* ((obj (make-instance 'change-class-class-03a :a 1))
	 (new-class (find-class 'change-class-class-03b))
	 (obj2 (make-instance new-class))
	 obj3)
    (slot-makunbound obj2 'a)
    (setf (slot-value obj2 'b) 17)
    (values
     (map-slot-boundp* obj2 '(a b))
     (eqt obj (change-class obj new-class))
     (typep* obj 'change-class-class-03a)
     (typep* obj 'change-class-class-03b)
     (typep* obj new-class)
     (eqt (setq obj3 (make-instance new-class)) obj)
     (map-slot-boundp* obj '(a b))
     (map-slot-boundp* obj2 '(a b))
     (map-slot-boundp* obj3 '(a b))
     (slot-value obj 'b)
     (slot-value obj2 'b)
     (slot-value obj3 'b)
     ))
  (nil t) t nil t t nil (nil t) (nil t) (nil t) 17 17 17)
