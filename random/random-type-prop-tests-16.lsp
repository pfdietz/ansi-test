;-*- Mode:     Lisp -*-

(in-package :cl-test)

(def-type-prop-test remove-duplicates.1 'remove-duplicates '(sequence) 1)
(def-type-prop-test remove-duplicates.2 'remove-duplicates '(list) 1)
(def-type-prop-test remove-duplicates.3
  'remove-duplicates
  '(sequence (eql :from-end) (member nil t))
  3)
(def-type-prop-test remove-duplicates.4
  'remove-duplicates
  '(sequence (eql :test) (member eql equal equalp))
  3)
(def-type-prop-test remove-duplicates.5
  'remove-duplicates
  '(sequence (eql :test-not) (member eql equal equalp))
  3)
(def-type-prop-test remove-duplicates.6
  'remove-duplicates
  '(sequence (member :test :test-not) (member eql equal equalp)
    (eql :from-end) (member nil t))
  5)
(def-type-prop-test remove-duplicates.7
  'remove-duplicates
  '(bit-vector (eql :key) (member 1+ 1- - #.#'1- #.#'1- #.#'-))
  3)
(def-type-prop-test remove-duplicates.8
  'remove-duplicates
  (list 'sequence
        '(eql :start) #'start-type-for-v)
  3)
(def-type-prop-test remove-duplicates.9
  'remove-duplicates
  (list 'sequence
        '(eql :end) #'end-type-for-v)
  3)
        
(def-type-prop-test remove-duplicates.10
  'remove-duplicates
  (list 'sequence
        '(eql :start) #'start-type-for-v
        '(eql :end) #'end-type-for-v)
  5)


