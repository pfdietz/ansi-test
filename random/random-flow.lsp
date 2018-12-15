;-*- Mode:     Lisp -*-

;;; Generate random control flow graph

(in-package :cl-test)

(defun make-random-control-flow (&key size num-nodes (num-local-vars 1))
  (let ((local-vars (loop for i from 1 

  
