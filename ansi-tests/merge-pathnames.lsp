;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Dec 31 11:25:55 2003
;;;; Contains: Tests of MERGE-PATHNAMES

(in-package :cl-test)

(deftest merge-pathnames.1
  (let* ((p1 (make-pathname :name "foo"))
	 (p2 (merge-pathnames p1 p1 nil)))
    (values
     (equalt (pathname-name p1) "foo")
     (if (equalt p1 p2) t
       (list p1 p2))))
  t t)

(deftest merge-pathnames.2
  (let* ((p1 (make-pathname :name "foo"))
	 (p2 (merge-pathnames p1 p1)))
    (values
     (equalt (pathname-host p1) (pathname-host p2))
     (equalt (pathname-device p1) (pathname-device p2))
     (equalt (pathname-directory p1) (pathname-directory p2))
     (equalt (pathname-name p1) (pathname-name p2))
     (equalt (pathname-name p1) "foo")
     (equalt (pathname-type p1) (pathname-type p2))
     (if (pathname-version p1)
	 (equalt (pathname-version p1) (pathname-version p2))
       (equalt (pathname-version p2) :newest))))
  t t t t t t t)

(deftest merge-pathnames.3
  (let* ((p1 (make-pathname :name "foo"))
	 (p2 (make-pathname :name "bar"))
	 (p3 (merge-pathnames p1 p2)))
    (values
     (equalt (pathname-host p1) (pathname-host p3))
     (equalt (pathname-device p1) (pathname-device p3))
     (equalt (pathname-directory p1) (pathname-directory p3))
     (equalt (pathname-name p1) (pathname-name p3))
     (equalt (pathname-name p1) "foo")
     (equalt (pathname-type p1) (pathname-type p3))
     (if (pathname-version p1)
	 (equalt (pathname-version p1) (pathname-version p3))
       (equalt (pathname-version p3) :newest))))
  t t t t t t t)

(deftest merge-pathnames.4
  (let* ((p1 (make-pathname :name "foo"))
	 (p2 (make-pathname :type "lsp"))
	 (p3 (merge-pathnames p1 p2)))
    (values
     (equalt (pathname-host p1) (pathname-host p3))
     (equalt (pathname-device p1) (pathname-device p3))
     (equalt (pathname-directory p1) (pathname-directory p3))
     (equalt (pathname-name p1) (pathname-name p3))
     (equalt (pathname-name p1) "foo")
     (equalt (pathname-type p3) "lsp")
     (equalt (pathname-type p2) (pathname-type p3))
     (if (pathname-version p1)
	 (equalt (pathname-version p1) (pathname-version p3))
       (equalt (pathname-version p3) :newest))))
  t t t t t t t t)

(deftest merge-pathnames.5
  (let* ((p1 (make-pathname :name "foo"))
	 (p2 (make-pathname :type "lsp" :version :newest))
	 (p3 (merge-pathnames p1 p2 nil)))
    (values
     (equalt (pathname-host p1) (pathname-host p3))
     (equalt (pathname-device p1) (pathname-device p3))
     (equalt (pathname-directory p1) (pathname-directory p3))
     (equalt (pathname-name p1) (pathname-name p3))
     (equalt (pathname-name p1) "foo")
     (equalt (pathname-type p3) "lsp")
     (equalt (pathname-type p2) (pathname-type p3))
     (equalt (pathname-version p1) (pathname-version p3))))
  t t t t t t t t)

(deftest merge-pathnames.6
  (let* ((p1 (make-pathname))
	 (p2 (make-pathname :name "foo" :version :newest))
	 (p3 (merge-pathnames p1 p2 nil)))
    (values
     (equalt (pathname-host p1) (pathname-host p3))
     (equalt (pathname-device p1) (pathname-device p3))
     (equalt (pathname-directory p1) (pathname-directory p3))
     (equalt (pathname-name p2) (pathname-name p3))
     (equalt (pathname-name p3) "foo")
     (equalt (pathname-type p2) (pathname-type p3))
     (pathname-version p2)
     (pathname-version p3)))
  t t t t t t :newest :newest)


  

