;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Apr 25 08:08:41 1998
;;;; Contains: Package test code, part 15

(in-package :cl-test)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; use-package

(deftest use-package-1
    (progn
      (ignore-errors (delete-package "H"))
      (ignore-errors (delete-package "G"))
      (let* ((pg (make-package "G" :use nil))
	     (ph (make-package "H" :use nil))
	     (sym1 (intern "FOO" pg)))
	(and
	 (eq (export sym1 pg) t)
	 (null (package-used-by-list pg))
	 (null (package-used-by-list ph))
	 (null (package-use-list pg))
	 (null (package-use-list ph))
	 (eq (use-package pg ph) t)  ;; "H" will use "G"
	 (multiple-value-bind (sym2 access)
	     (find-symbol "FOO" ph)
	   (and
	    (eq access :inherited)
	    (eq sym1 sym2)))
	 (equal (package-use-list ph) (list pg))
	 (equal (package-used-by-list pg) (list ph))
	 (null (package-use-list pg))
	 (null (package-used-by-list ph))
	 (eq (unuse-package pg ph) t)
	 (null (find-symbol "FOO" ph)))))
  t)

(deftest use-package-2
    (progn
      (ignore-errors (delete-package "H"))
      (ignore-errors (delete-package "G"))
      (let* ((pg (make-package "G" :use nil))
	     (ph (make-package "H" :use nil))
	     (sym1 (intern "FOO" pg)))
	(and
	 (eq (export sym1 pg) t)
	 (null (package-used-by-list pg))
	 (null (package-used-by-list ph))
	 (null (package-use-list pg))
	 (null (package-use-list ph))
	 (eq (use-package "G" "H") t)  ;; "H" will use "G"
	 (multiple-value-bind (sym2 access)
	     (find-symbol "FOO" ph)
	   (and
	    (eq access :inherited)
	    (eq sym1 sym2)))
	 (equal (package-use-list ph) (list pg))
	 (equal (package-used-by-list pg) (list ph))
	 (null (package-use-list pg))
	 (null (package-used-by-list ph))
	 (eq (unuse-package pg ph) t)
	 (null (find-symbol "FOO" ph)))))
  t)

(deftest use-package-3
    (progn
      (ignore-errors (delete-package "H"))
      (ignore-errors (delete-package "G"))
      (let* ((pg (make-package "G" :use nil))
	     (ph (make-package "H" :use nil))
	     (sym1 (intern "FOO" pg)))
	(and
	 (eq (export sym1 pg) t)
	 (null (package-used-by-list pg))
	 (null (package-used-by-list ph))
	 (null (package-use-list pg))
	 (null (package-use-list ph))
	 (eq (use-package '#:|G| '#:|H|) t)  ;; "H" will use "G"
	 (multiple-value-bind (sym2 access)
	     (find-symbol "FOO" ph)
	   (and
	    (eq access :inherited)
	    (eq sym1 sym2)))
	 (equal (package-use-list ph) (list pg))
	 (equal (package-used-by-list pg) (list ph))
	 (null (package-use-list pg))
	 (null (package-used-by-list ph))
	 (eq (unuse-package pg ph) t)
	 (null (find-symbol "FOO" ph)))))
  t)

(deftest use-package-4
    (progn
      (ignore-errors (delete-package "H"))
      (ignore-errors (delete-package "G"))
      (let* ((pg (make-package "G" :use nil))
	     (ph (make-package "H" :use nil))
	     (sym1 (intern "FOO" pg)))
	(and
	 (eq (export sym1 pg) t)
	 (null (package-used-by-list pg))
	 (null (package-used-by-list ph))
	 (null (package-use-list pg))
	 (null (package-use-list ph))
	 (eq (ignore-errors (use-package #\G #\H))
	     t)  ;; "H" will use "G"
	 (multiple-value-bind (sym2 access)
	     (find-symbol "FOO" ph)
	   (and
	    (eq access :inherited)
	    (eq sym1 sym2)))
	 (equal (package-use-list ph) (list pg))
	 (equal (package-used-by-list pg) (list ph))
	 (null (package-use-list pg))
	 (null (package-used-by-list ph))
	 (eq (unuse-package pg ph) t)
	 (null (find-symbol "FOO" ph)))))
  t)

;; use lists of packages

(deftest use-package-5
    (let ((pkgs '("H" "G1" "G2" "G3"))
	  (vars '("FOO1" "FOO2" "FOO3")))
      (dolist (p pkgs)
	(ignore-errors (delete-package p))
	(make-package p :use nil))
      (and
       (every (complement #'package-use-list) pkgs)
       (every (complement #'package-used-by-list) pkgs)
       (every #'(lambda (v p)
		  (export (intern v p) p))
	      vars (cdr pkgs))
       (progn
	 (dolist (p (cdr pkgs)) (intern "MINE" p))
	 (eq (use-package (cdr pkgs) (car pkgs)) t))
       (every #'(lambda (v p)
		  (eq (find-symbol v p)
		      (find-symbol v (car pkgs))))
	      vars (cdr pkgs))
       (null (find-symbol "MINE" (car pkgs)))
       (every #'(lambda (p)
		  (equal (package-used-by-list p)
			 (list (find-package (car pkgs)))))
	      (cdr pkgs))
       (equal (sort-package-list (package-use-list (car pkgs)))
	      (mapcar #'find-package (cdr pkgs)))
       (every (complement #'package-use-list) (cdr pkgs))
       (null (package-used-by-list (car pkgs)))))
  t)

;; Circular package use

(deftest use-package-6
    (progn
      (ignore-errors (delete-package "H"))
      (ignore-errors (delete-package "G"))
      (let ((pg (make-package "G"))
	    (ph (make-package "H"))
	    sym1 sym2 sym3 sym4
	    a1 a2 a3 a4)
	(prog1
	    (and
	     (export (intern "X" pg) pg)
	     (export (intern "Y" ph) ph)
	     (use-package pg ph)
	     (use-package ph pg)
	     (progn
	       (multiple-value-setq
		   (sym1 a1) (find-symbol "X" pg))
	       (multiple-value-setq
		   (sym2 a2) (find-symbol "Y" ph))
	       (multiple-value-setq
		   (sym3 a3) (find-symbol "Y" pg))
	       (multiple-value-setq
		   (sym4 a4) (find-symbol "X" ph))
	       (and
		(eq a1 :external)
		(eq a2 :external)
		(eq a3 :inherited)
		(eq a4 :inherited)
		(eq sym1 sym4)
		(eq sym2 sym3)
		(eq (symbol-package sym1) pg)
		(eq (symbol-package sym2) ph)
		(unuse-package pg ph)
		(unuse-package ph pg))))
	  (ignore-errors (delete-package pg))
	  (ignore-errors (delete-package ph)))))
  t)

;; Also: need to check that *PACKAGE* is used as a default
