;-*- Mode:     Lisp -*-

(in-package :cl-test)

(def-type-prop-test coerce.1 '(lambda (x) (coerce x 'sequence))
  '(sequence)
  1)

(def-type-prop-test coerce.2 '(lambda (x) (coerce x 'vector))
  '(sequence)
  1)

(def-type-prop-test coerce.3 '(lambda (x) (coerce x 'character))
  '(character)
  1)

(def-type-prop-test coerce.4 '(lambda (x) (coerce (string x) 'character))
  '(character)
  1)

(def-type-prop-test coerce.5 '(lambda (x) (coerce (make-symbol (string x)) 'character))
  '(character)
  1)

(def-type-prop-test coerce.6 '(lambda (x) (coerce x 'complex))
  '(number)
  1)

(def-type-prop-test coerce.7 'coerce
  '(t (eql t))
  2)







                    
