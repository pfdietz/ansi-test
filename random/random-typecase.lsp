(in-package :cl-test)

(defun loop-random-typecase (n reps &key (type t))
  (loop
    repeat reps
    do (let* ((e (make-random-element-of-type type))
              (nc (1+ (random n)))
              (clauses
                (loop for i from 1 to nc
                      collect `(,(make-random-type-not-containing e) ,i)))
              (form
                `(typecase x ,@clauses))
              (lam `(lambda (x) ,form)))
         (cl:handler-bind
             (#+sbcl (sb-ext::compiler-note #'muffle-warning)
              (warning #'muffle-warning))
           (cl:handler-case
               (let ((result (funcall (compile nil lam) e)))
                 (when result
                   (return (list lam e result))))
             (error (e)
               (return (list lam e))))))))


         
         
         
