;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Mar 21 22:28:53 2003
;;;; Contains: Tests for RESTART-BIND

(in-package :cl-test)

(deftest restart-bind.1
  (restart-bind () nil)
  nil)

(deftest restart-bind.2
  (restart-bind () (values)))
  
(deftest restart-bind.3
  (restart-bind () (values 'a 'b 'c 'd 'e 'f))
  a b c d e f)

(deftest restart-bind.4
  (block nil
    (restart-bind () (return 'good) 'bad))
  good)

(deftest restart-bind.5
  (block done
    (tagbody
     (restart-bind () (go 10) (return-from done 'bad))
     10
     (return-from done 'good)))
  good)

(deftest restart-bind.6
  (restart-bind ())
  nil)

(deftest restart-bind.7
  (block done
    (restart-bind ((foo #'(lambda () (return-from done 'good))))
		  (invoke-restart 'foo)
		  'bad))
  good)

(deftest restart-bind.8
  (block done
    (restart-bind ((foo #'(lambda () (return-from done 'good))))
		  (let ((restart (find-restart 'foo)))
		    (and (typep restart 'restart)
			 (invoke-restart restart)))
		  'bad))
  good)

(deftest restart-bind.9
  (restart-bind ((foo #'(lambda (a b c) (list c a b))))
		(invoke-restart 'foo 1 2 3))
  (3 1 2))

(deftest restart-bind.10
  (flet ((%f () (invoke-restart 'foo 'x 'y 'z)))
    (restart-bind ((foo #'(lambda (a b c) (list c a b))))
		  (%f)))
  (z x y))

(deftest restart-bind.11
  (restart-bind
   ((foo #'(lambda () 'bad)))
   (restart-bind
    ((foo #'(lambda () 'good)))
    (invoke-restart 'foo)))
  good)

(deftest restart-bind.12
  (let ((*x* 'bad))
    (declare (special *x*))
    (restart-bind
     ((foo #'(lambda () (declare (special *x*)) *x*)))
     (let ((*x* 'good))
       (declare (special *x*))
       (invoke-restart 'foo))))
  good)

(deftest restart-bind.13
  (restart-bind
   ((foo #'(lambda () 'bad)))
   (flet ((%f () (invoke-restart 'foo)))
     (restart-bind
      ((foo #'(lambda () 'good)))
      (%f))))
  good)





