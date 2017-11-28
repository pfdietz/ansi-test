;-*- Mode:     Lisp -*-

(in-package :cl-test)

(def-type-prop-test replace.1 'replace
  (list 'list 'list)
  2 :replicate '(t nil))

(def-type-prop-test replace.2 'replace
  (list 'bit-vector 'bit-vector)
  2 :replicate '(t nil))

(def-type-prop-test replace.3 'replace
  (list 'list 'bit-vector)
  2 :replicate '(t nil))

(def-type-prop-test replace.4 'replace
  (list 'string 'base-string)
  2 :replicate '(t nil))

(def-type-prop-test replace.4a 'replace
  (list '(vector character) 'string)
  2 :replicate '(t nil))

(def-type-prop-test replace.5 'replace
  (list 'list 'string)
  2 :replicate '(t nil))

(def-type-prop-test replace.6 'replace
  (list '(vector t) 'vector)
  2 :replicate '(t nil))

(def-type-prop-test replace.7 'replace
  (list 'list 'vector)
  2 :replicate '(t nil))

(def-type-prop-test replace.8 'replace
  (list '(vector t) 'list)
  2 :replicate '(t nil))

(def-type-prop-test replace.9 'replace
  (list '(vector t) 'bit-vector)
  2 :replicate '(t nil))

(def-type-prop-test replace.10 'replace
  (list '(vector t) 'bit-vector)
  2 :replicate '(t nil))

(def-type-prop-test replace.11 'replace
  (list '(or list (vector t))
        'sequence
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type)
  4 :replicate '(t nil nil nil))

(def-type-prop-test replace.12 'replace
  (list 'bit-vector
        'bit-vector
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type)
  4 :replicate '(t nil nil nil))
  
(def-type-prop-test replace.13 'replace
  (list 'base-string
        'base-string
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type)
  4 :replicate '(t nil nil nil))

(def-type-prop-test replace.14 'replace
  (list '(vector character)
        'string
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type)
  4 :replicate '(t nil nil nil))

;;

(def-type-prop-test replace.15 'replace
  (list '(or list (vector t))
        'sequence
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type)
  6 :replicate '(t nil nil nil nil nil))

(def-type-prop-test replace.16 'replace
  (list 'bit-vector
        'bit-vector
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type)
  6 :replicate '(t nil nil nil nil nil))
  
(def-type-prop-test replace.17 'replace
  (list 'base-string
        'base-string
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type)
  6 :replicate '(t nil nil nil nil nil))

(def-type-prop-test replace.18 'replace
  (list '(vector character)
        'string
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type)
  6 :replicate '(t nil nil nil nil nil))

;;

(def-type-prop-test replace.19 'replace
  (list '(or list (vector t))
        'sequence
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type)
  8 :replicate '(t nil nil nil nil nil nil nil))

(def-type-prop-test replace.20 'replace
  (list 'bit-vector
        'bit-vector
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type)
  8 :replicate '(t nil nil nil nil nil nil nil))
  
(def-type-prop-test replace.21 'replace
  (list 'base-string
        'base-string
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type)
  8 :replicate '(t nil nil nil nil nil nil nil))

(def-type-prop-test replace.22 'replace
  (list '(vector character)
        'string
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type
        '(member :start1 :start2 :end1 :end2)
        #'start-end-type)
  8 :replicate '(t nil nil nil nil nil nil nil))
