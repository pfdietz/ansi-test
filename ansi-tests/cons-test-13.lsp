;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:38:57 1998
;;;; Contains: Testing of CL Features related to "CONS", part 13

(in-package :cl-test)
(use-package :rt)
(declaim (optimize (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; member

(deftest member-1
    (let* ((x (copy-tree '(a b c d e f)))
	   (xcopy (make-scaffold-copy x))
	   (result (member 'c x)))
      (and
       (eqt result (cddr x))
       (check-scaffold-copy x xcopy)))
  t)

(deftest member-2
    (let* ((x (copy-tree '(a b c d e f)))
	   (xcopy (make-scaffold-copy x))
	   (result (member 'e x)))
      (and
       (eqt result (cddddr x))
       (check-scaffold-copy x xcopy)))
  t)

(deftest member-3
    (let* ((x (copy-tree '(1 2 3 4 5 6 7)))
	   (xcopy (make-scaffold-copy x))
	   (result (member 4 x)))
      (and
       (eqt result (cdddr x))
       (check-scaffold-copy x xcopy)))
  t)

(deftest member-4
    (let* ((x (copy-tree '(2 4 6 8 10 12)))
	   (xcopy (make-scaffold-copy x))
	   (result (member 9 x :key #'1+)))
      (and
       (eqt result (cdddr x))
       (check-scaffold-copy x xcopy)))
  t)

(deftest member-5
    (let* ((x (copy-tree '((a b) (c d) (e f) (g h))))
	   (xcopy (make-scaffold-copy x))
	   (result (member '(c d) x :test #'equal)))
      (and
       (eqt result (cdr x))
       (check-scaffold-copy x xcopy)))
  t)

(deftest member-6
    (let* ((x (copy-tree '((a b) (c d) (e f) (g h))))
	   (xcopy (make-scaffold-copy x))
	   (result (member 'c x :key #'car)))
      (and
       (eqt result (cdr x))
       (check-scaffold-copy x xcopy)))
  t)

(deftest member-7
    (let* ((x (copy-tree '((a b) (c d) (e f) (g h))))
	   (xcopy (make-scaffold-copy x))
	   (result (member 'c x :key #'car :test #'eq)))
      (and
       (eqt result (cdr x))
       (check-scaffold-copy x xcopy)))
  t)

(deftest member-8
    (let* ((x (copy-tree '((a b) (c d) (e f) (g h))))
	   (xcopy (make-scaffold-copy x))
	   (result (member 'c x :key #'car :test-not (complement #'eq))))
      (and
       (eqt result (cdr x))
       (check-scaffold-copy x xcopy)))
  t)

(deftest member-9
    (let* ((x (copy-tree '((a b) (c d) (e f) (g h))))
	   (xcopy (make-scaffold-copy x))
	   (result (member 'c x :key #'car :test #'eql)))
      (and
       (eqt result (cdr x))
       (check-scaffold-copy x xcopy)))
  t)

(deftest member-10
    (let* ((x (copy-tree '((a b) (c d) (e f) (g h))))
	   (xcopy (make-scaffold-copy x))
	   (result (member (list 'd) x :key #'cdr :test #'equal)))
      (and
       (eqt result (cdr x))
       (check-scaffold-copy x xcopy)))
  t)

(deftest member-11
    (member (copy-seq "cc") (copy-tree '("aa" "bb" "cc" "dd" "ee")))
  nil)

(deftest member-12
    (member 1 (copy-tree '(3 4 1 31 423)))
  (1 31 423))

(deftest member-13
    (member (copy-seq "cc") (copy-tree '("aa" "bb" "cc" "dd" "ee"))
	    :test #'equal)
  ("cc" "dd" "ee"))

(deftest member-14
  (member 'a nil)
  nil)

(deftest member-15
  (member nil nil)
  nil)

(deftest member-16
  (member nil nil :test #'equal)
  nil)

(deftest member-16-a
  (member nil nil :test #'(lambda (x y) (error "Should not call this function")))
  nil)

(deftest member-17
  (member 'a nil :test #'(lambda (x y) (error "Should not call this function")))
  nil)

;; Check that a null key argument is ignored

(deftest member-18
  (member 'a '(c d a b e) :key nil)
  (a b e))

(deftest member-19
  (member 'z '(a b c d) :key nil)
  nil)

;;; Keyword tests

(deftest member.allow-other-keys.1
  (member 'b '(a b c) :bad t :allow-other-keys t)
  (b c))

(deftest member.allow-other-keys.2
  (member 'b '(a b c) :allow-other-keys t :bad t)
  (b c))

(deftest member.allow-other-keys.3
  (member 'b '(a b c) :allow-other-keys t)
  (b c))

(deftest member.allow-other-keys.4
  (member 'b '(a b c) :allow-other-keys nil)
  (b c))

(deftest member.allow-other-keys.5
  (member 'b '(a b c) :allow-other-keys 17 :allow-other-keys nil '#:x t)
  (b c))

(deftest member.keywords.6
  (member 'b '(a b c) :test #'eq :test (complement #'eq))
  (b c))

;;; Error cases

(deftest member.error.1
  (catch-type-error (member 'a 'b))
  type-error)

(deftest member.error.2
  (catch-type-error (member 'a 1.3))
  type-error)

(deftest member.error.3
  (catch-type-error (member 'a 1))
  type-error)

(deftest member.error.4
  (catch-type-error (member 'a 0))
  type-error)

(deftest member.error.5
  (catch-type-error (member 'a "abcde"))
  type-error)

(deftest member.error.6
  (catch-type-error (member 'a #\w))
  type-error)

(deftest member.error.7
  (catch-type-error (member 'a t))
  type-error)

(deftest member.error.8
  (classify-error (member))
  program-error)

(deftest member.error.9
  (classify-error (member nil))
  program-error)

(deftest member.error.10
  (classify-error (member nil nil :bad t))
  program-error)

(deftest member.error.11
  (classify-error (member nil nil :test))
  program-error)

(deftest member.error.12
  (classify-error (member nil nil :bad t :allow-other-keys nil))
  program-error)

(deftest member.error.13
  (classify-error (member nil nil nil))
  program-error)
