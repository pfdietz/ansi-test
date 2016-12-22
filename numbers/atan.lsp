;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Feb 11 06:01:55 2004
;;;; Contains: Tests of ATAN



(deftest atan.1
  (let ((result (atan 0)))
    (or (eqlt result 0)
        (eqlt result 0.0)))
  t)

(deftest atan.2
  (loop for type in '(short-float single-float double-float long-float)
        for zero = (coerce 0 type)
        unless (eql (atan zero) zero)
        collect type)
  nil)

(deftest atan.3
  (loop for type in '(short-float single-float double-float long-float)
        for zero = (coerce 0 type)
        unless (eql (atan zero 1) zero)
        collect type)
  nil)

(deftest atan.4
  (loop for type in '(short-float single-float double-float long-float)
        for zero = (coerce 0 type)
        for one = (coerce 1 type)
        unless (eql (atan 0 one) zero)
        collect type)
  nil)

(deftest atan.5
  (loop for type in '(short-float single-float double-float long-float)
        for zero = (coerce 0 type)
        for one = (coerce 1 type)
        unless (eql (atan zero one) zero)
        collect type)
  nil)

(deftest atan.6
  (loop for type in '(short-float single-float double-float long-float)
        for a = (coerce 2000 type)
        for b = (coerce -1000 type)
        collect
        (loop for x = (- (random a) b)
              for rlist = (multiple-value-list (atan x))
              for y = (car rlist)
              repeat 1000
              unless (and (null (cdr rlist))
                          (typep y type))
              collect (list x rlist)))
  (nil nil nil nil))

(deftest atan.7
  (loop for type in '(short-float single-float double-float long-float)
        for a = (coerce 2000 type)
        for b = (coerce -1000 type)
        for zero = (coerce 0 type)
        collect
        (loop for x = (- (random a) b)
              for rlist = (multiple-value-list (atan (complex x zero)))
              for y = (car rlist)
              repeat 1000
              unless (and (null (cdr rlist))
                          (typep y `(complex ,type)))
              collect (list x rlist)))
  (nil nil nil nil))

(deftest atan.8
  (loop for type in '(short-float single-float double-float long-float)
        for a = (coerce 2000 type)
        for b = (coerce -1000 type)
        for zero = (coerce 0 type)
        collect
        (loop for x = (- (random a) b)
              for rlist = (multiple-value-list (atan (complex zero x)))
              for y = (car rlist)
              repeat 1000
              unless (and (null (cdr rlist))
                          (typep y `(complex ,type)))
              collect (list x rlist)))
  (nil nil nil nil))

(deftest atan.9
  (loop for type in '(short-float single-float double-float long-float)
        for a = (coerce 2000 type)
        for b = (coerce -1000 type)
        for zero = (coerce 0 type)
        collect
        (loop for x1 = (- (random a) b)
              for x2 = (- (random a) b)
              for rlist = (multiple-value-list (atan (complex x1 x2)))
              for y = (car rlist)
              repeat 1000
              unless (and (null (cdr rlist))
                          (typep y `(complex ,type)))
              collect (list x1 x2 rlist)))
  (nil nil nil nil))

(deftest atan.10
  (approx= (atan 1) (coerce (/ pi 4) 'single-float))
  t)

(deftest atan.11
  (loop for type in '(short-float single-float double-float long-float)
        collect (approx= (atan (coerce 1 type)) (coerce (/ pi 4) type)))
  (t t t t))

(deftest atan.12
  (approx= (atan -1) (coerce (/ pi -4) 'single-float))
  t)

(deftest atan.13
  (loop for type in '(short-float single-float double-float long-float)
        collect (approx= (atan (coerce -1 type)) (coerce (/ pi -4) type)))
  (t t t t))

(deftest atan.14
  (macrolet ((%m (z) z)) (atan (expand-in-current-env (%m 0.0))))
  0.0)

;;; FIXME
;;; More accuracy tests here

;;; ieee-fp tests
(deftest atan.ieee.1 :description "Verify if atan handles 0.0 correctly"
  ;; (atan +-0 +(anything-but-nan))  -> +-0
  ;; (atan +-0 -(anything-but-nan))  -> +-pi
  (flet ((pip (a b)
           ;; we are not testing accuracy, so this simplified
           ;; precdicate would be sufficient for our needs. We are
           ;; also not interested in sign of the result, because we
           ;; don't know whenever zero is signed or not.
           (<= (abs (- (abs a) (abs b))) 0.01)))
    (every #'identity
           (append
            (map 'list (lambda (n)
                         ;; notice, that we don't test a case, where
                         ;; both arguments are 0.0, because if
                         ;; implementation doesn't support signed 0
                         ;; result is undefined.
                         (and (zerop (atan -0.0 n))
                              (zerop (atan +0.0 n))
                              (pip pi (atan +0.0 (- n)))
                              (pip pi (atan -0.0 (- n)))))
                 (remove-if-not #'plusp *floats*))
            ;; (atan +-(anything-but-0/nan) 0) -> +-pi/2
            (map 'list (lambda (n)
                         (and (pip (* +1/2 pi) (atan n +0.0))
                              (pip (* -1/2 pi) (atan n -0.0))))
                 (remove-if #'zerop *floats*)))))
  T)

;;; We could have tested also for infinities and signed 0, but there
;;; is no portable ieee-fp, we could put it in ansi-beyond test suite
;;; though:
;;;
;;;   (atan +0.0 -anything-but-0/nan/inf)   -> +pi
;;;   (atan -0.0 -anything-but-0/nan/inf)   -> -pi
;;;   (atan +0.0 +0.0)                      -> +0.0
;;;   (atan -0.0 +0.0)                      -> -0.0
;;;   (atan +0.0 -0.0)                      -> +pi
;;;   (atan -0.0 -0.0)                      -> -pi
;;;   (atan (anything) nan)                 -> nan
;;;   (atan nan (anything))                 -> nan
;;;   (atan +-inf +inf)                     -> +-pi/4
;;;   (atan +-inf -inf)                     -> +-3pi/4
;;;   (atan +-(anything-but-inf/nan), +inf) -> +-0
;;;   (atan +-(anything-but-inf/nan), -inf) -> +-pi
;;;   (atan +-inf (anything-but-0/nan/inf)) -> +-pi/2
;;;
;;; (deftest atan.ieee.2 t t)

;;; Error tests

(deftest atan.error.1
  (signals-error (atan) program-error)
  t)

(deftest atan.error.2
  (signals-error (atan 1 1 1) program-error)
  t)

(deftest atan.error.3
  (check-type-error #'atan #'numberp)
  nil)

(deftest atan.error.4
  (check-type-error #'(lambda (x) (atan x 1)) #'realp)
  nil)

(deftest atan.error.5
  (check-type-error #'(lambda (x) (atan 1 x)) #'realp)
  nil)
