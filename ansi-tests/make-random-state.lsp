;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep  6 17:53:30 2003
;;;; Contains: Tests of MAKE-RANDOM-STATE

(in-package :cl-test)

(deftest make-random-state.error.1
  (classify-error (make-random-state nil nil))
  program-error)

(deftest make-random-state.error.2
  (classify-error (make-random-state t nil))
  program-error)

(deftest make-random-state.error.3
  (classify-error (make-random-state *random-state* nil))
  program-error)

(deftest make-random-state.error.4
  (loop for x in *mini-universe*
	unless (or (null x) (eq x t) (random-state-p x)
		   (eq (eval `(classify-error (make-random-state ',x)))
		       'type-error))
	collect x)
  nil)

(deftest make-random-state.1
  (let ((rs (make-random-state)))
    (and (not (eq rs *random-state*))
	 (random-state-p rs)
	 (eqlt (random 1000000) (random 1000000 rs))))
  t)

(deftest make-random-state.2
  (let ((rs (make-random-state *random-state*)))
    (and (not (eq rs *random-state*))
	 (random-state-p rs)
	 (eqlt (random 1000000) (random 1000000 rs))))
  t)

(deftest make-random-state.3
  (let ((rs (make-random-state)))
    (random 10)
    (let ((rs2 (make-random-state rs)))
      (and (not (eq rs *random-state*))
	   (not (eq rs rs2))
	   (not (eq rs2 *random-state*))
	   (random-state-p rs)
	   (random-state-p rs2)
	   (eqlt (random 1.0 rs) (random 1.0 rs2)))))
  t)

(deftest make-random-state.4
  (let ((rs (make-random-state t))
	(rs2 (make-random-state t)))
    (and (random-state-p rs)
	 (not (eq rs *random-state*))
	 (random-state-p rs2)
	 (not (eq rs2 *random-state*))
	 (not (eq rs rs2))
	 (integerp (random 10 rs))
	 (floatp (random 1.0 rs2))
	 t))
  t)

  
