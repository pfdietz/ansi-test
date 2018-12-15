;-*- Mode:     Lisp -*-

(in-package :cl-test)

;;; Special case test generator for dynamic-extent

;; Function used on DX objects
(defun dxf (x i) (elt x i))
(declaim (notinline dxf))

(defun make-top-level-dynamic-extent-form (size)
  (let* ((list-form (make-random-list-form-for-dynamic-extent size))
         (tag ''c)
         (*random-int-form-catch-tags*
          (cons tag *random-int-form-catch-tags*))
         (var 'v)
         (result (random-selector-of-dynamic-extent list-form var)))
    `(catch ,tag
       (let ((,var ,list-form))
         (declare (dynamic-extent ,var))
         ;; (dxf ,var ,i)
         ,result
         ))))

(defun random-selector-of-dynamic-extent (form var)
  (if (consp form)
      (case (car form)
        ((list vector)
         (let ((n (length (cdr form))))
           (if (= n 0)
               var
               (let* ((i (random n))
                      (arg (elt (cdr form) i)))
                 (random-selector-of-dynamic-extent
                  arg `(dxf ,var ,i))))))
        (t var))
      var))

(defun make-random-list-form-for-dynamic-extent (size)
  (let* ((nforms (1+ (random (max 1 (min size 4)))))
         (sizes (random-partition (max 1 (1- size)) nforms))
         (args (loop for s in sizes
                  collect (if (or (< s 4)
                                  (not (= (random 4) 0)))
                              (make-random-integer-form s)
                              (make-random-list-form-for-dynamic-extent s))))
         (op (random-from-seq '(list vector))))
    `(,op ,@args)))


        
