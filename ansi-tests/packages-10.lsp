;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:03:36 1998
;;;; Contains: Package test code, part 10

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with-package-iterator

(defconstant +fail-count-limit+ 20)

(defmacro test-with-package-iterator (package-list-expr &rest symbol-types)
  "Build an expression that tests the with-package-iterator form."
  (let ((name (gensym))
	(cht-var (gensym))
	(pkg-list-var (gensym)))
    `(let ((,cht-var (make-hash-table))
	   (,pkg-list-var ,package-list-expr)
	   (fail-count 0))
	 (with-package-iterator (,name ,pkg-list-var
				       ,@(copy-list symbol-types))
	   ;; For each symbol, check that name is returning appropriate
	   ;; things
	   (loop
	     (block fail
	       (multiple-value-bind (more sym access pkg)
		   (,name)
		 (unless more (return nil))
		 (setf (gethash sym ,cht-var) t)  ;; note presence of symbol
		 ;; Check that its access status is in the list,
		 ;;  that pkg is a package,
		 ;;  that the symbol is in the package,
		 ;;  and that (in the package) it has the correct access type
		 (unless (member access (quote ,(copy-list symbol-types)))
		   (unless (> fail-count +fail-count-limit+)
		     (format t "Bad access type: ~S ==> ~A~%" sym access))
		   (when (= fail-count +fail-count-limit+)
		     (format t "Further messages suppressed~%"))
		   (incf fail-count)
		   (return-from fail nil))
		 
		 (unless (packagep pkg)
		   (unless (> fail-count +fail-count-limit+)
		     (format t "Not a package: ~S ==> ~S~%" sym pkg))
		   (when (= fail-count +fail-count-limit+)
		     (format t "Further messages suppressed~%"))
		   (incf fail-count)
		   (return-from fail nil))
		 (multiple-value-bind (sym2 access2)
		     (find-symbol (symbol-name sym) pkg)
		   (unless (or (eq sym sym2)
			       (member sym2 (PACKAGE-SHADOWING-SYMBOLS pkg)))
		     (unless (> fail-count +fail-count-limit+)
		       (format t "Not same symbol: ~S ~S~%" sym sym2))
		     (when (= fail-count +fail-count-limit+)
		       (format t "Further messages suppressed~%"))
		     (incf fail-count)
		     (return-from fail nil))
		   (unless  (eq access access2)
		     (unless (> fail-count +fail-count-limit+)
		       (format t "Not same access type: ~S ~S ~S~%"
			       sym access access2))
		     (when (= fail-count +fail-count-limit+)
		       (format t "Further messages suppressed~%"))
		     (incf fail-count)
		     (return-from fail nil)))))))
	 ;; now, check that each symbol in each package has
	 ;; been properly found
	 (loop
	     for p in ,pkg-list-var do
	       (block fail
		 (do-symbols (sym p)
		   (multiple-value-bind (sym2 access)
		       (find-symbol (symbol-name sym) p)
		     (unless (eq sym sym2)
		       (unless (> fail-count +fail-count-limit+)
			 (format t "Not same symbol (2): ~S ~S~%"
				 sym sym2))
		       (when (= fail-count +fail-count-limit+)
			 (format t "Further messages suppressed~%"))
		       (incf fail-count)
		       (return-from fail nil))
		     (unless (or (not (member access
					      (quote ,(copy-list symbol-types))))
				 (gethash sym ,cht-var))
		       (format t "Symbol not found: ~S~%" sym)
		       (incf fail-count)
		       (return-from fail nil))))))
	 (or (zerop fail-count) fail-count))))

(defun with-package-iterator-internal (packages)
  (test-with-package-iterator packages :internal))

(defun with-package-iterator-external (packages)
  (test-with-package-iterator packages :external))

(defun with-package-iterator-inherited (packages)
  (test-with-package-iterator packages :inherited))

(defun with-package-iterator-all (packages)
  (test-with-package-iterator packages :internal :external :inherited))

(deftest with-package-iterator-1
    (with-package-iterator-internal (list (find-package "COMMON-LISP-USER")))
  t)

(deftest with-package-iterator-2
    (with-package-iterator-external (list (find-package "COMMON-LISP-USER")))
  t)

(deftest with-package-iterator-3
    (with-package-iterator-inherited (list (find-package "COMMON-LISP-USER")))
  t)

(deftest with-package-iterator-4
    (with-package-iterator-all (list (find-package "COMMON-LISP-USER")))
  t)

;; Should test on some packages containing shadowed symbols,
;; multiple inheritance

(deftest with-package-iterator-5
    (handler-case
	(with-package-iterator-all '("A"))
      (error (c) c))
  t)

(deftest with-package-iterator-6
    (handler-case
	(with-package-iterator-all '(#:|A|))
      (error (c) c))
  t)

(deftest with-package-iterator-7
    (handler-case
	(with-package-iterator-all '(#\A))
      (error (c) c))
  t)

(deftest with-package-iterator-8
    (handler-case
	(with-package-iterator-internal (list (find-package "A")))
      (error (c) c))
  t)

(deftest with-package-iterator-9
    (handler-case
	(with-package-iterator-external (list (find-package "A")))
      (error (c) c))
  t)

(deftest with-package-iterator-10
    (handler-case
	(with-package-iterator-inherited (list (find-package "A")))
      (error (c) c))
  t)

;; Check that if no access symbols are provided, a program error is
;; raised
#|
(deftest with-package-iterator-11
    (handler-case
	(progn
	  (test-with-package-iterator (list (find-package "COMMON-LISP-USER")))
	  nil)
      (program-error () t)
      (error (c) c))
  t)
|#

;; Paul Werkowski" <pw@snoopy.mv.com> pointed out that
;; test is broken.  Here's a version of the replacement'
;; he suggested.
;;
;; I'm not sure if this is correct either; it depends on
;; whether with-package-iterator should signal the error
;; at macro expansion time or at run time.
;;
(deftest with-package-iterator-11
    (handler-case (macroexpand-1
		   '(with-package-iterator (x "COMMON-LISP-USER")))
      (program-error () t)
      (error (c) c))
  t)

;; Apply to all packages
(deftest with-package-iterator-12
    (handler-case
	(loop
	    for p in (list-all-packages) count
	      (handler-case
		  (progn
		    (format t "Package ~S~%" p)
		    (not (with-package-iterator-internal (list p))))
		(error (c)
		  (format "Error ~S on package ~A~%" c p)
		  t)))
      (error (c) c))
  0)

(deftest with-package-iterator-13
    (handler-case
	(loop
	    for p in (list-all-packages) count
	      (handler-case
		  (progn
		    (format t "Package ~S~%" p)
		    (not (with-package-iterator-external (list p))))
		(error (c)
		  (format "Error ~S on package ~A~%" c p)
		  t)))
      (error (c) c))
  0)

(deftest with-package-iterator-14
    (handler-case
	(loop
	    for p in (list-all-packages) count
	      (handler-case
		  (progn
		    (format t "Package ~S~%" p)
		    (not (with-package-iterator-inherited (list p))))
		(error (c)
		  (format t "Error ~S on package ~S~%" c p)
		  t)))
      (condition (c) c))
  0)
