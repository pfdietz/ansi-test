(in-package :cl-test)

(defclass rtp-class-1 ()
  ((x :accessor rtp-x :initarg :x :initform (random 100))))

(defclass rtp-class-2 (rtp-class-1)
  ((y :accessor rtp-y :initarg :y :initform (random 100))))

(defclass rtp-class-3 (rtp-class-1)
  ((z :accessor rtp-z :initarg :z :initform (random 100))))

(defclass rtp-class-4 (rtp-class-2 rtp-class-3)
  ((w :accessor rtp-w :initarg :w :initform (random 100))))

(defmethod make-random-element-of-type ((type (eql 'rtp-class-1)))
  (make-instance 'rtp-class-1))

(defmethod make-random-element-of-type ((type (eql 'rtp-class-2)))
  (make-instance 'rtp-class-2))

(defmethod make-random-element-of-type ((type (eql 'rtp-class-3)))
  (make-instance 'rtp-class-3))

(defmethod make-random-element-of-type ((type (eql 'rtp-class-4)))
  (make-instance 'rtp-class-4))

