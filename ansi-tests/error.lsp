;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jan 28 21:37:43 2003
;;;; Contains: Tests of ERROR

(in-package :cl-test)

(deftest error.1
  (let ((fmt "Error"))
    (handler-case (error fmt)
		  (simple-error (c)
				(eqt (simple-condition-format-control c)
				     fmt))))
  t)

(deftest error.2
  (let* ((fmt "Error")
	 (cnd (make-condition 'simple-error :format-control fmt)))
    (handler-case (error cnd)
		  (simple-error (c)
				(and (eqt c cnd)
				     (eqt (simple-condition-format-control c)
					  fmt)))))
  t)

(deftest error.3
  (let ((fmt "Error"))
    (handler-case (error 'simple-error :format-control fmt)
		  (simple-error (c)
				(eqt (simple-condition-format-control c)
				     fmt))))
  t)



