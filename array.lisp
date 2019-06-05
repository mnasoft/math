;;;; array.lisp

(in-package #:math)

(defmethod row ((row integer) (a array))
  "
Тестирование:
 (row -1 (make-array '(5 2) :initial-contents '((0 1)(2 3)(4 5)(6 7)(8 9)))) 
 (row  5 (make-array '(5 2) :initial-contents '((0 1)(2 3)(4 5)(6 7)(8 9)))) 
 (row  0 (make-array '(5 2) :initial-contents '((0 1)(2 3)(4 5)(6 7)(8 9)))) #(0 1)
 (row  2 (make-array '(5 2) :initial-contents '((0 1)(2 3)(4 5)(6 7)(8 9)))) #(4 5)
 (row  4 (make-array '(5 2) :initial-contents '((0 1)(2 3)(4 5)(6 7)(8 9)))) #(8 9)
"
  (assert (= (array-rank a) 2))
  (assert (< -1 row (array-dimension a 0)))
  (apply #'vector (loop :for j :from 0 :below (array-dimension a 1) :collect
		       (aref a row j))))

(defmethod col ((col integer) (a array))
  "
Тестирование:
 (col -1 (make-array '(5 2) :initial-contents '((0 1)(2 3)(4 5)(6 7)(8 9)))) 'error
 (col  2 (make-array '(5 2) :initial-contents '((0 1)(2 3)(4 5)(6 7)(8 9)))) 'error
 (col  0 (make-array '(5 2) :initial-contents '((0 1)(2 3)(4 5)(6 7)(8 9)))) #(0 2 4 6 8)
 (col  1 (make-array '(5 2) :initial-contents '((0 1)(2 3)(4 5)(6 7)(8 9)))) #(1 3 5 7 9)

"
  (assert (= (array-rank a) 2))
  (assert (< -1 col (array-dimension a 1)))
  (apply #'vector (loop :for i :from 0 :below (array-dimension a 0) :collect
		       (aref a i col))))

(defun make-vector-n (element n)
  "Пример использования:
 (make-vector-n 1.5 3) => #(1.5 1.5 1.5)"
  (make-array (list n) :initial-element element))

