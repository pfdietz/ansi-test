(in-package :cl-test)

(defpackage :cl-test/notinline
  (:use)
  (:export :if :cond :while :unless))

(defun ifc (f1 f2 f3)
  (if (funcall f1)
      (funcall f2)
      (funcall f3)))

(defmacro cl-test/notinline:if (condition then-part &optional else-part)
  (let ((f1 (gensym "F"))
        (f2 (gensym "F"))
        (f3 (gensym "F")))

    `(flet ((,f1 () ,condition)
            (,f2 () ,then-part)
            (,f3 () ,else-part))
       (declare (notinline ,f1 ,f2 ,f3))
       (ifc #'f1 #'f2 #'f3))))

(defmacro cl-test/notinline:when (condition &body body)
  (let ((f1 (gensym "F"))
        (f2 (gensym "F")))
    `(flet ((,f1 () ,condition)
            (,f2 () ,@body))
       (declare (notinline ,f1 ,f2))
       (ifc #'f1 #'f2 #'(lambda () nil)))))

(defmacro cl-test/notinline:unless  (condition &body body)
  (let ((f1 (gensym "F"))
        (f2 (gensym "F")))
    `(flet ((,f1 () ,condition)
            (,f2 () ,@body))
       (declare (notinline ,f1 ,f2))
       (ifc #'f1 #'(lambda () nil) #'f2))))

(defmacro cl-test/notline:cond (clauses)
  (cond
    ((null clauses) nil)
    (t
     (destructuring-bind (condition . body) (car clauses)
       `(cl-test/notinline:if ,condition (progn ,@body)
                              (cl-test/notinline:cond ,@(cdr clauses)))))))
