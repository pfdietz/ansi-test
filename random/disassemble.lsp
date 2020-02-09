(in-package :cl-test)

;;; Random testing of DISASSEMBLE

(defparameter *disassemble-lambda* nil)

(defun test-one-random-disassemble (size nvars &key (int-form-fn #'make-random-integer-form))
  (let* ((vars (make-vars nvars))
         (var-types (make-var-types nvars))
         (opts (make-random-optimize-settings))
         (form (make-form size :int-form-fn int-form-fn :vars vars
                          :var-types var-types :nvars nvars))
         (lam
          `(lambda ,vars (declare (optimize ,@opts)) ,form)))
    (setf *disassemble-lambda* lam)
    (disassemble-test-fn form vars var-types nil opts nil)))

(defun disassemble-test-fn (form vars var-types vals-list opt-decls-1
                            opt-decls-2)
  (declare (ignore vals-list opt-decls-2))
  (let ((lam `(lambda ,vars (declare (optimize ,@opt-decls-1)) ,form)))
    (block done
      (let ((*standard-output* (make-broadcast-stream)))
        (handler-bind
            (#+sbcl
             (sb-ext::compiler-note #'muffle-warning)
             (warning #'muffle-warning)
             #+(or)
             (type-error
              (lambda (c)
                (when
                    (and (equal (type-error-expected-type c)
                                '(unsigned-byte 16))
                         (equal (type-error-datum c) -1))
                  (return-from done nil))))
             ((and (or error serious-condition)
                   #+sbcl (not sb-sys:interactive-interrupt))
              (lambda (c)
                (return-from done
                  `(:form ,form :vars ,vars :var-types ,var-types :opt-decls-1 ,opt-decls-1 :lam ,lam :condition ,c)))))
          (disassemble lam)))
      nil)))

(defun dtf1 (r)
  (disassemble-test-fn (getf r :form) (getf r :vars) (getf r :var-types)
                        nil (getf r :opt-decls-1) nil))

(defun test-random-disassemble (size nvars reps &rest args)
  (loop for i from 0 below reps
     do (when (= (mod (1+ i) 100) 0) (format t ".") (finish-output))
     do (let ((result (apply #'test-one-random-disassemble size nvars args)))
          (when result
            
            (return result)))))

(defun prune-disassemble (r)
  (let ((form (getf r :form))
        (vars (getf r :vars))
        (var-types (getf r :var-types))
        (opt-decls-1 (getf r :opt-decls-1)))
    (prune-int-form form vars var-types nil opt-decls-1 nil :test-fn #'disassemble-test-fn)))
