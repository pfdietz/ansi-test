;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jul  4 07:17:52 2004
;;;; Contains: Tests of PPRINT-LOGICAL-BLOCK

(in-package :cl-test)

(deftest pprint-logical-block.1
  (with-standard-io-syntax
   (let ((*print-pretty* t))
     (with-open-stream
      (os (make-string-output-stream))
      (values
       (multiple-value-list (pprint-logical-block (os 1)))
       (get-output-stream-string os)))))
  (nil) "1")




  
