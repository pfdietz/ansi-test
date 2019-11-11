(in-package :cl-test)

;;; Random test case generator for case forms

#|
(defun make-random-case-form (size arg-names arg-vals
                              &key (type 'integer)
                                (cutoff 5))
  (let ((arg-count (length arg-names)))
    (assert (= arg-count (length arg-vals)))
    (assert (every (lambda (s) (typep a type)) arg-vals))
    (flet ((%cvls (n)
             (let ((m (1+ (random n)))
                   (val-lists
                    (loop repeat m
                       collect
                         (make-random-element-of-type type))))
               (loop repeat (- n m)
                  do (let ((i (random m)))
                       (push (make-random-element-of-type type)
                             (elt val-lists i))))))
           (%r ()
             (rcase
              (1 (make-random-element-of-type type))
              (1 random-from-seq arg-names))))
      (if (<= size cutoff)
          ;; Generate a single case form
          (let ((case-value-lists (%cvls size)))
            `(case (random-from-seq arg-names)
               ,@(loop for cvl in case-lists
                    collect
                      `(,cvl ,(%r)))
               (t ,(%r))))
          ;; Generate nested case forms
          (let* ((case-num (1+ (random cutoff)))
                 |#

(defun make-simple-random-case-form (size arg-names arg-vals
                                     &key (type 'integer) (kind 'case)
                                       (p 0.1)
                                       (result-type 'integer)
                                       &allow-other-keys)
                                       
  "Generate a simple single-level case/ecase/ccase form of
the indicated type.  ARG-NAMES is a list of symbols that are
free variables.  ARG-VALS are the values of the variables."
  (assert (typep kind '(member case ecase ccase)))
  (assert (typep arg-names 'list))
  (assert (typep arg-vals 'list))
  (assert (typep size '(integer 1)))
  (assert (= (length arg-names) (length arg-vals)))
  (assert (typep (car arg-vals) type))
  (flet ((%key ()
           ;; Generate a random key
           (if (<= (random 1.0) p)
               (car arg-vals)
               (make-random-element-of-type type)))
         (%v () (make-random-element-of-type result-type)))
  (let* ((num-clauses (1+ (random size)))
         (clause-sizes (random-partition size num-clauses)))
    `(,kind
      ,(car arg-names)
      ,@(loop for clause-size in clause-sizes
           collect
             `(,(loop repeat clause-size collect (%key))
                ,(%v)))
      ,@(when (and (eql kind 'case) (coin))
          `((t ,(%v))))))))

(defun eval-simple-case-form (form arg-names arg-vals)
  (flet ((%ev (x)
           (cond
             ((constantp x) x)
             ((symbolp x)
              (loop for s in arg-names
                 for v in arg-vals
                 when (eql s x) do (return v)
                 finally (error "Name not found: ~a" x)))
             (t (error "Cannot evaluate ~a" x)))))
    (let ((v (%ev (cadr form))))
      (loop for (keys result-form) in (cddr form)
         when (or (and (eql (car form) 'case)
                       (member keys '(t otherwise)))
                  (member v keys))
         return (%ev result-form)
         finally (if (eql (car form) 'case)
                     nil
                     (error "Fell through ~a form" (car form)))))))

(defun check-case-form (form arg-names arg-vals arg-type)
  (let* ((expected (eval-simple-case-form form arg-names arg-vals))
         (lam `(lambda ,arg-names
                 ,@(when (coin) `((declare (type ,arg-type ,(car arg-names)))))
                 ,form))
         (actual (handler-bind
                     ((warning #'muffle-warning)
                      #+sbcl (sb-ext::compiler-note #'muffle-warning)
                      )
                   (apply (compile nil lam) arg-vals))))
    (if (eql expected actual)
        nil
        (list expected actual lam))))

(defun single-random-case-test (size n-args &rest key-args &key (type 'integer) &allow-other-keys)
  (assert (typep n-args '(integer 1 20)))
  (let* ((arg-names (subseq '#.(loop for i from 1 to 20
                                  collect (intern (format nil "V~A" i) :cl-test))
                            0 n-args))
         (arg-vals (loop repeat n-args collect (make-random-element-of-type type)))
         (form (apply #'make-simple-random-case-form size arg-names arg-vals key-args)))
    (check-case-form form arg-names arg-vals type)))

(defun random-case-test (size n-args &rest args &key (reps 1000) &allow-other-keys)
  (loop for i from 1 to reps
     do (progn
          (when (= (mod i 100) 0)
            (format t "~a" i)
            (if (= (mod i 2000) 0)
                (terpri)
                (format t " "))
            (finish-output))
          (let ((result (apply #'single-random-case-test size n-args args)))
            (when result (return result))))))
