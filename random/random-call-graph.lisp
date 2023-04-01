(in-package :cl-test)

;;; Generate a set of random functions in a LABELS form,
;;; with conditionals and calls to other functions there,
;;; or ERROR forms.

(defun make-random-call-graph-form (&key (n-funs 10) (n-vars 8)
                                         (degree 3) (spread 2)
                                      (error-prob 0.1))
  (let ((vars (loop for i from 1 to n-vars
                    collect (intern (format nil "V~a" i) :cl-test)))
        (funs (loop for i from 1 to n-funs
                    collect (intern (format nil "F~a" i) :cl-test)))
        (ecount 0))
    (labels ((body (size)
               ;; Generate a random fun body of size SIZE
               (if (<= size 1)
                   (if (<= (random 1.0) error-prob)
                       `(error ,(format nil "~a" (incf ecount)))
                       (rcase
                         (1 (list (random-from-seq funs)))
                         (1 (random-from-seq vars))
                         (1 (random 1000))))
                   (let* ((s1 (1+ (random (1- size))))
                          (s2 (- size s1)))
                     (assert (> s1 0))
                     (assert (> s2 0))
                     `(if ,(random-from-seq vars)
                          (,(random-from-seq funs))
                          (,(random-from-seq funs)))))))
      
      `(lambda ,vars
         (declare (ignorable ,@vars))
         (labels ,(loop for f in funs
                        collect (let ((s (+ (random (1+ (* spread 2)))
                                            (- spread) degree)))
                                  `(,f () ,(body s))))
           (declare (ignorable ,@(loop for f in funs collect `(function ,f))))
           (,(car funs)))))))

(defun test-random-call-graph (reps)
  (loop for i from 1 to reps
        for n = (1+ (random 20))
        for d = (1+ (random 5))
        for error-prob = (min (random 1.0) (random 1.0) (random 1.0) (random 1.0))
        for spread = (random d)
        do (let ((form (make-random-call-graph-form :n-funs n :degree d :error-prob error-prob
                                                    :spread spread)))
             (declare (special *form*))
             (setf *form* form)
             (cl:handler-bind
                 (#+sbcl (sb-ext::compiler-note #'muffle-warning)
                  (warning #'muffle-warning))
               (compile nil form)))
        do (when (= (nth-value 1 (floor i 100)) 0)
             (format t " ~a" i)
             (finish-output)
             (when (= (nth-value 1 (floor i 2000)) 0)
               (terpri)))))

  

