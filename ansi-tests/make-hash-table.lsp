;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jan 26 21:36:33 2003
;;;; Contains: Tests for MAKE-HASH-TABLE

(in-package :cl-test)

(deftest make-hash-table.1
  (let ((ht (make-hash-table)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.2
  (let ((ht (make-hash-table :size 0)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.3
  (let ((ht (make-hash-table :size 100)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.4
  (let ((ht (make-hash-table :test #'eq)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.5
  (let ((ht (make-hash-table :test 'eq)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.6
  (let ((ht (make-hash-table :test #'eql)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.7
  (let ((ht (make-hash-table :test 'eql)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.8
  (let ((ht (make-hash-table :test #'equal)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.9
  (let ((ht (make-hash-table :test 'equal)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.10
  (let ((ht (make-hash-table :test #'equalp)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)

(deftest make-hash-table.11
  (let ((ht (make-hash-table :test 'equalp)))
    (values
     (notnot (typep ht 'hash-table))
     (notnot (hash-table-p ht))
     (hash-table-count ht)))
  t t 0)











     
    
