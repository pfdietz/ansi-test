;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Apr 28 21:56:47 2003
;;;; Contains: Tests for REINITIALIZE-INSTANCE

(in-package :cl-test)

;;; Many of the classes used here are defined in defclass-??.lsp

(deftest reinitialize-instance.1
  (let* ((obj (make-instance 'class-01))
	 (obj2 (reinitialize-instance obj)))
    (values
     (eqt obj obj2)
     (map-slot-boundp* obj '(s1 s2 s3))))
  t (nil nil nil))


(deftest reinitialize-instance.2
  (let* ((obj (make-instance 'class-01))
	 (obj2 (reinitialize-instance obj :allow-other-keys nil)))
    (values
     (eqt obj obj2)
     (map-slot-boundp* obj '(s1 s2 s3))))
  t (nil nil nil))

(deftest reinitialize-instance.3
  (let* ((obj (make-instance 'class-01))
	 (obj2 (reinitialize-instance obj :allow-other-keys t)))
    (values
     (eqt obj obj2)
     (map-slot-boundp* obj '(s1 s2 s3))))
  t (nil nil nil))

(deftest reinitialize-instance.4
  (let* ((obj (make-instance 'class-01))
	 (obj2 (reinitialize-instance obj :allow-other-keys t
				      :allow-other-keys nil)))
    (values
     (eqt obj obj2)
     (map-slot-boundp* obj '(s1 s2 s3))))
  t (nil nil nil))

(deftest reinitialize-instance.5
  (let* ((obj (make-instance 'class-07))
	 (obj2 (reinitialize-instance obj :s1a 'a :s2 'b :s1a 'bad
				      :s2 'bad2  :s1b 'bad3)))
    (values
     (eqt obj obj2)
     (map-slot-value obj '(s1 s2))))
  t (a b))

(deftest reinitialize-instance.6
  (let* ((obj (make-instance 'class-07 :s1a 'a))
	 (obj2 (reinitialize-instance obj :s1b 'b)))
    (values
     (eqt obj obj2)
     (slot-value obj 's1)
     (slot-boundp* obj 's2)))
  t b nil)

(deftest reinitialize-instance.7
  (let* ((obj (make-instance 'class-07 :s1a 'a))
	 (obj2 (reinitialize-instance obj :s2 'b)))
    (values
     (eqt obj obj2)
     (slot-value obj 's1)
     (slot-value obj 's2)))
  t a b)











