;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep 29 20:49:47 2002
;;;; Contains: Tests for REMOVE-DUPLICATES, DELETE-DUPLICATES

(in-package :cl-test)

(deftest random-remove-duplicates
  (loop for i from 1 to 5000
	always (random-test-remove-dups 20))
  t)

(deftest random-delete-duplicates
  (loop for i from 1 to 5000
	always (random-test-remove-dups 20 nil))
  t)

;;; Look for :KEY NIL bugs

(deftest remove-duplicates.1
  (let* ((orig '(1 2 3 4 1 3 4 1 2 5 6 2 7))
	 (x (copy-seq orig))
	 (y (remove-duplicates x :key nil)))
    (and (equalp orig x) y))
  (3 4 1 5 6 2 7))

(deftest delete-duplicates.1
  (let* ((orig '(1 2 3 4 1 3 4 1 2 5 6 2 7))
	 (x (copy-seq orig))
	 (y (delete-duplicates x :key nil)))
    y)
  (3 4 1 5 6 2 7))

;;; Error cases

(deftest remove-duplicates.error.1
  (classify-error (remove-duplicates))
  program-error)

(deftest remove-duplicates.error.2
  (classify-error (remove-duplicates nil :start))
  program-error)

(deftest remove-duplicates.error.3
  (classify-error (remove-duplicates nil 'bad t))
  program-error)

(deftest remove-duplicates.error.4
  (classify-error (remove-duplicates nil 'bad t :allow-other-keys nil))
  program-error)

(deftest remove-duplicates.error.5
  (classify-error (remove-duplicates nil 1 2))
  program-error)

(deftest delete-duplicates.error.1
  (classify-error (delete-duplicates))
  program-error)

(deftest delete-duplicates.error.2
  (classify-error (delete-duplicates nil :start))
  program-error)

(deftest delete-duplicates.error.3
  (classify-error (delete-duplicates nil 'bad t))
  program-error)

(deftest delete-duplicates.error.4
  (classify-error (delete-duplicates nil 'bad t :allow-other-keys nil))
  program-error)

(deftest delete-duplicates.error.5
  (classify-error (delete-duplicates nil 1 2))
  program-error)






