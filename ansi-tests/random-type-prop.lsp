;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Dec 23 20:39:22 2004
;;;; Contains: Randomized tests of type propagation in the compiler

(in-package :cl-test)

(eval-when (load eval compile)
  (compile-and-load "random-int-form.lsp"))

(defgeneric make-random-type-containing (val)
  (:documentation
   "Given a value, generate a random type that contains that value."))

(defun do-random-type-prop-tests (&key operator
				       (minargs 2)
				       (maxargs minargs)
				       (arg-types nil)
				       (rest-type 'integer)
				       (reps 1000)
				       (enclosing-the nil))
  (assert (<= 1 minargs maxargs 20))
  (loop
   repeat reps
   do
   (let* ((param-names
	   '(p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
	     p11 p12 p13 p14 p15 p16 p17 p18 p19 p20))
	  (nargs (+ minargs (random (- maxargs minargs -1))))
	  (types (append arg-types
			 (make-list (- nargs (length arg-types))
				    :initial-element rest-type)))
	  (vals (mapcar #'make-random-element-of-type types))
	  (is-var? (loop repeat (length vals) collect (coin)))
	  (params (loop for x in is-var?
			for p in param-names
			when x collect p))
	  (param-types (mapcar #'make-random-type-containing vals))
	  (type-decls (loop for x in is-var?
			    for p in param-names
			    for tp in param-types
			    when x
			    collect `(type ,tp ,p)))
	  (result2 (multiple-value-list (apply operator vals)))
	  (result-type (if (and enclosing-the (integerp (car result2)))
			   (make-random-type-containing (car result2))
			 t))
	  (expr `(,operator ,@(loop for x in is-var?
				    for v in vals
				    for p in param-names
				    collect (if x
						(rcase
						 (1 (let ((tp (make-random-type-containing v)))
						      `(the ,tp ,p)))
						 (1 p))
					      v))))
	  (form
	   `(lambda ,params
	      (declare (optimize speed (safety 1))
		       ,@ type-decls)
	      ,(if enclosing-the `(the ,result-type ,expr) expr)))
	  (param-vals (loop for x in is-var?
			    for v in vals
			    when x collect v))
	  (fn (cl:handler-bind
	       (#+sbcl (sb-ext::compiler-note #'muffle-warning)
		       (warning #'muffle-warning))
	       (compile nil form)))
	  (result1 (multiple-value-list (apply fn param-vals))))
     (unless (equal result1 result2)
       (return (list :form form
		     :vals vals
		     :result1 result1
		     :result2 result2))))))

(defmethod make-random-type-containing ((val integer))
  (rcase
   (1 (random-from-seq '(t number real rational)))
   (1 (random-from-seq '(integer signed-byte)))
   (1 `(eql ,val))
   (1 (rcase (1 `(member ,(make-random-integer) ,val))
	     (1 `(member ,val ,(make-random-integer)))))
   (2 (let ((lo (abs (make-random-integer))))
	`(integer ,(- val lo))))
   (2 (let ((lo (abs (make-random-integer))))
	`(integer ,(- val lo) *)))
   (2 (let ((hi (abs (make-random-integer))))
	`(integer * ,(+ val hi))))
   (4 (let ((lo (abs (make-random-integer)))
	    (hi (abs (make-random-integer))))
	`(integer ,(- val lo) ,(+ val hi))))
   (1 (if (>= val 0) `unsigned-byte
	(make-random-type-containing val)))))


	  
	
	  
	  
	  
		 
