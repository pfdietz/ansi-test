;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat May 17 11:54:54 2003
;;;; Contains: Tests of MAKE-LOAD-FORM-SAVING-SLOTS

(in-package :cl-test)

;;; These are tests of MAKE-LOAD-FORM-SAVING-SLOTS proper; tests involving
;;; file compilation will be located elsewhere.


(defstruct mlfss-01 a b c)

(deftest make-load-form-saving-slots.1
  (let* ((obj (make-mlfss-01))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj))))
    (values
     (length forms)
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (eqt (class-of obj) (class-of newobj)))))
  2 t)

(deftest make-load-form-saving-slots.2
  (let* ((obj (make-mlfss-01))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj :slot-names '(a b)))))
    (values
     (length forms)
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (eqt (class-of obj) (class-of newobj)))))
  2 t)


(defclass mlfss-02 () ((a :initarg :a) (b :initarg :b) (c :initarg :c)))

(deftest make-load-form-saving-slots.3
  (let* ((obj (make-instance 'mlfss-02))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj))))
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (values
	(length forms)
	(eqt (class-of obj) (class-of newobj))
	(map-slot-boundp* newobj '(a b c)))))
  2 t (nil nil nil))

(deftest make-load-form-saving-slots.4
  (let* ((obj (make-instance 'mlfss-02 :a 1 :b 'a :c '(x y z)))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj :slot-names '(a b c)))))
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (values
	(length forms)
	(eqt (class-of obj) (class-of newobj))
	(map-slot-boundp* newobj '(a b c))
	(map-slot-value newobj '(a b c)))))
  2 t (t t t) (1 a (x y z)))


(deftest make-load-form-saving-slots.5
  (let* ((obj (make-instance 'mlfss-02 :a #(x y z)))
	 (forms (multiple-value-list
		 (make-load-form-saving-slots obj :slot-names '(a b)))))
     (let ((newobj (eval (first forms))))
       (eval (subst newobj obj (second forms)))
       (values
	(length forms)
	(eqt (class-of obj) (class-of newobj))
	(map-slot-boundp* newobj '(a b c))
	(slot-value newobj 'a))))
  2 t (t nil nil) #(x y z))


