;;;; array.lisp

(in-package #:math)
(annot:enable-annot-syntax)

@export
(defmethod rows ((a array))
  (assert (= (array-rank a) 2))
  (array-dimension a 0))

@export
(defmethod cols ((a array))
  (assert (= (array-rank a) 2))
  (array-dimension a 1))

@export
(defmethod row ((row integer) (a array))
  "@b(Описание:) метод @b(row) возвращает строку @b(row) из масства @d(a).
Строка возвращается в виде вектора vector.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((arr (make-array '(5 2)
			 :initial-contents '((0 1)
					     (2 3)
					     (4 5)
					     (6 7)
					     (8 9))))
	(arr-1 (make-array '(5 2 3) :initial-element 0)))

;;;(row -1 arr)             ; => error
;;;(row  5 arr)             ; => error
   (row  0 arr)		    ;=> #(0 1)
   (row  2 arr)	            ;=> #(4 5)
   (row  4 arr)		    ;=> #(8 9)
   (row 2 arr-1)
   )
@end(code)
"
  (when (/= (array-rank a) 2) (error 'row-operation-not-appicable))
  (assert (< -1 row (array-dimension a 0)))
  (apply #'vector (loop :for j :from 0 :below (array-dimension a 1) :collect
								    (aref a row j))))

@export
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

(defmethod (setf row) ((new-row cons) (a array) row)
  (assert (= (array-rank a) 2))
  (assert (< -1 row (rows a)))
  (assert (= (cols a) (length new-row)))
  (let ((ll new-row))
    (loop :for c :from 0 :below (cols a)
       :do (setf (aref a row c) (car ll)
		 ll (cdr ll)))
    a))

(defmethod (setf col) ((new-col cons) (a array) col )
  (assert (= (array-rank a) 2))
  (assert (< -1 col (cols a)))
  (assert (= (rows a) (length new-col)))
  (let ((ll new-col))
    (loop :for r :from 0 :below (rows a)
       :do (setf (aref a r col) (car ll)
		 ll (cdr ll)))
    a))

(defmethod (setf row) ((new-row vector) (a array) row)
  (assert (= (array-rank a) 2))
  (assert (< -1 row (rows a)))
  (assert (= (cols a) (length new-row)))
  (loop :for c :from 0 :below (cols a) :do (setf (aref a row c) (svref new-row c)))
  a)

(defmethod (setf col) ((new-col vector) (a array) col )
  (assert (= (array-rank a) 2))
  (assert (< -1 col (cols a)))
  (assert (= (rows a) (length new-col)))
  (loop :for r :from 0 :below (rows a) :do (setf (aref a r col) (svref new-col r)))
  a)

(defmethod (setf row) ((new-val number) (a array) row)
  (assert (= (array-rank a) 2))
  (assert (< -1 row (rows a)))
  (loop :for c :from 0 :below (cols a) :do (setf (aref a row c) new-val))
  a)

(defmethod (setf col) ((new-val number) (a array) col )
  (assert (= (array-rank a) 2))
  (assert (< -1 col (cols a)))
  (loop :for r :from 0 :below (rows a) :do (setf (aref a r col) new-val))
  a)

(defun make-vector-n (element n)
  "Пример использования:
 (make-vector-n 1.5 3) => #(1.5 1.5 1.5)"
  (make-array (list n) :initial-element element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *a* (make-array '(2 3) :initial-contents '((1 2 3)(4 5 6))))
