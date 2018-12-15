(in-package :cl-test)

;;; Utility to make a random tagbody

(defun make-random-digraph (n m)
  "Create a random digraph with n nodes and m edges.  Error if m is out of range of possible values."
  (assert (typep n '(integer 0)))
  (assert (typep m '(integer 0)))
  (let ((m-max (* n (1- n)))
        (adj-vec (make-array (list n) :initial-element nil)))
    (assert (<= m m-max))
    (if (> m (/ m-max 2))
        ;; Generate all and select randomly
        (let ((m-vec (make-array (list m-max)))
              (k 0))
          (loop for i from 0 below n do
               (loop for j from 0 below n do
                    (unless (= i j)
                      (setf (aref m-vec k) (list i j))
                      (incf k))))
          (assert (= k m-max))
          (select-m-random-edges m-vec m adj-vec))
        ;; Few edges; generate with iteration when collision
        ;; is detected
        (generate-random-edges n adj-vec m #'/=))
    (adj-vec-to-lists n adj-vec)))
    
(defun select-m-random-edges (m-vec m adj-vec)
  (let ((k (length m-vec)))
    (loop repeat m
       do (let ((r (random k)))
            (destructuring-bind (i j) (aref m-vec r)
              (push j (aref adj-vec i)))
            (setf (aref m-vec r) (aref m-vec (decf k)))))))

(defun adj-vec-to-lists (n adj-vec)
  (assert (= (length adj-vec) n))
  (loop for i from 0 below n
     collect (cons i (sort (aref adj-vec i) #'<))))

(defun generate-random-edges (n adj-vec m pred)
  (let ((e-table (make-hash-table :test #'equal)))
    (loop for i from 0 below n do
         (loop for j in (aref adj-vec i) do
              (setf (gethash (list i j) e-table) t)))
    (loop repeat m
       do (loop (let* ((i (random n))
                       (j (random n)))
                  (when (funcall pred i j)
                    (let ((e (list i j)))
                      (unless (gethash e e-table)
                        (setf (gethash e e-table) t)
                        (push j (aref adj-vec i))
                        (return)))))))))

(defun make-random-mostly-dag (n m b)
  "Create a random DAG in which m edges go forward and b edges go backwards"
  (assert (typep n '(integer 0)))
  (assert (typep m '(integer 0)))
  (assert (typep b '(integer 0)))
  (let ((m-max (/ (* n (1- n)) 2))
        (adj-vec (make-array (list n) :initial-element nil)))
    (assert (<= m m-max))
    (assert (<= b m-max))
    (if (> m (/ m-max 2))
        ;; Generate all and select randomly
        (let* ((m-vec (coerce
                       (loop for i from 0 below n nconc
                            (loop for j from (1+ i) below n do
                               collect (list i j)))
                       'vector)))
          (select-m-random-edges m-vec m adj-vec))
        ;; Generate randomly
        (generate-random-edges n adj-vec m #'<))
    ;; Now generate back edges
    (generate-random-edges n adj-vec b #'>)
    (adj-vec-to-lists n adj-vec)))
