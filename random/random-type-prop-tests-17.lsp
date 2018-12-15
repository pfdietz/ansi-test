;-*- Mode:     Lisp -*-

(in-package :cl-test)

(def-type-prop-test nth-value.1
  'nth-value
  (list `(integer 0 (,multiple-values-limit)) t)
  2)

(def-type-prop-test nth-value.2
  'nth-value
  (list '(integer 0 (32)) t)
  2)

(def-type-prop-test nth-value.3
  '(lambda (i x) (nth-value i (apply #'values x)))
  (list '(integer 0 (32)) 'list)
  2)

(def-type-prop-test nth-value.4
  '(lambda (i x) (nth-value i (apply #'values x)))
  (list '(integer 0 (64)) 'list)
  2)

(def-type-prop-test nth-value.5
  '(lambda (i x y z) (nth-value i (values x y z)))
  (list `(or (integer 0 (5)) (integer 0 (,multiple-values-limit))) t t t)
  4)

(def-type-prop-test funcall.1
  'funcall
  (list `(member ,@(loop for x in '(+ * - 1+ 1- = /=)
                      append (list x (symbol-function x))))
        'number)
  2)

(def-type-prop-test funcall.2
  'funcall
  (list `(member ,@(loop for x in '(+ * - = /=) append (list x (symbol-function x))))
        'number
        'number)
  3)

(def-type-prop-test funcall.3
  '(lambda (x) (funcall (constantly x)))
  (list t)
  1)


