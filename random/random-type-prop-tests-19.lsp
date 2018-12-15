;-*- Mode:     Lisp -*-

(in-package :cl-test)

(def-type-prop-test sin.1 'sin '(real) 1)
(def-type-prop-test sin.2 'sin '(single-float) 1)
(def-type-prop-test sin.3 'sin '(double-float) 1)
(def-type-prop-test sin.4 'sin '(long-float) 1)
(def-type-prop-test sin.5 'sin '(short-float) 1)
(def-type-prop-test cos.1 'cos '(real) 1)
(def-type-prop-test cos.2 'cos '(single-float) 1)
(def-type-prop-test cos.3 'cos '(double-float) 1)
(def-type-prop-test cos.4 'cos '(long-float) 1)
(def-type-prop-test cos.5 'cos '(short-float) 1)
(def-type-prop-test tan.1 'tan '((real (#.(- (/ pi 2))) (#.(/ pi 2)))) 1)
