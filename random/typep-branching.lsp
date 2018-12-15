(in-package :cl-test)

(defun call-until (n fn &rest args)
  ;; Call FN n times with args ARGS, returning
  ;; first non-null result
  (loop repeat n
     do (let ((result (apply fn args)))
          (when result (return result)))))

(defun typep-branching (s)
  (let* ((name 'x)
         (v (make-random-element-of-type t))
         (args
          (loop repeat s
             collect
               (let* ((then? (coin))
                      (tp (make-random-type-containing
                           (make-random-element-of-type t)))
                      (e `(typep ,name ',tp)))
                 (if (typep v tp)
                     (if then?
                         `(if ,e 'good 'bad)
                         `(if (not ,e) 'bad 'good))
                     (if then?
                         `(if ,e 'bad 'good)
                         `(if (not ,e) 'good 'bad)))))))
    (cl:handler-bind
        (#+sbcl (sb-ext::compiler-note #'muffle-warning)
                (warning #'muffle-warning))
      (let* ((lam `(lambda (,name)
                     (list ,@args)))
             (result (funcall (compile nil lam) v)))
        (unless (equal result (make-list s :initial-element 'good))
          (list lam v result))))))


      
      
                       

