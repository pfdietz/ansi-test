(in-package :cl-test)

;;; Special purpose random tester for programs of the form
;;;
;;; (defun f (x)
;;;  (declare (type (integer I1 I2) x))
;;;  (typep x '(integer I3 I4)))
;;;
;;; where (<= I1 x I2) and (<= I3 I4)

(defun typep-integer-test (n)
  ;; Test for n iterations, return non-nil on test failure
  (loop repeat n
     do  (let* ((i1 (make-random-element-of-type 'integer))
                (i2 (make-random-element-of-type `(integer ,i1)))
                (p (make-random-element-of-type `(integer ,i1 ,i2)))
                (i3 (make-random-element-of-type 'integer))
                (i4 (make-random-element-of-type `(integer ,i3)))
                (form
                 `(lambda (x)
                    (declare (type (integer ,i1 ,i2) x))
                    (typep x '(integer ,i3 ,i4)))))
           (unless (eql (not (<= i3 p i4))
                        (not (funcall (compile nil form) p)))
             (return (list i1 i2 i3 i4 p form))))))

(defun typep-real-test (n &key (types '(integer float short-float single-float
                                        long-float double-float real rational)))
  (loop repeat n
       do (let* ((base (random-from-seq types))
                 (x1 (make-random-element-of-type base))
                 (x2 (make-random-element-of-type base)))
            (when (> x1 x2) (rotatef x1 x2))
            (let* ((t1 `(,base ,x1 ,x2))
                   (x (make-random-element-of-type t1)))
              (assert (typep x t1))
              (let* ((base2 (random-from-seq types))
                     (x3 (make-random-element-of-type base2))
                     (x4 (make-random-element-of-type base2)))
                (when (> x3 x4)
                  (rotatef x3 x4))
                (let* ((t2 `(,base2 ,x3 ,x4))
                       (form
                        `(lambda (x)
                           (declare (type ,t1 x))
                           (typep x ',t2)))
                       (compiled-form (compile nil form))
                       (result (funcall compiled-form x)))
                  (unless (if (typep x base2)
                              (eql (not (<= x3 x x4))
                                   (not result))
                              (not result))
                    (return (list x1 x2 x3 x4 x form result)))))))))


(defun subtypep-check-test (n m)
  (loop repeat n
     do (let* ((x (make-random-element-of-type 'real))
               (t1 (make-random-type-containing x))
               (t2 (make-random-type-containing x)))
          (multiple-value-bind (sub good?)
              (subtypep t1 t2)
            (when (and sub good?)
                  (loop repeat m
                     do
                       (handler-case
                           (let ((y (make-random-element-of-type t1)))
                             (unless (typep y t2)
                               (return-from subtypep-check-test
                                 (list x t1 t2 y))))
                         (internal-test-failure () (return nil))
                         (error (e)
                           (return-from subtype-check-test
                             (list x t1 t2 y e))))))))))


                   
