;;;; ./src/matr/defmethod.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; mref
(defmethod mref ((mm <matrix>) i j)
  (aref (matrix-data mm) i j))

(defmethod mref ((mm cons) i j)
  (nth j (nth i mm)))

(defmethod mref ((mm array) i j)
  (aref mm i j))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; mref
(defmethod (setf mref) (value (mm <matrix>) i j)
  (setf (aref (matrix-data mm) i j) value)
  mm)

(defmethod (setf mref) (value (mm cons) i j)
  (setf (nth j (nth i mm)) value)
  mm)

(defmethod (setf mref) (value (mm array) i j)
  (setf (aref mm i j) value)
  mm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; transpose

(defmethod transpose ((mm <matrix>))
  (let ((rez (make-instance '<matrix> :dimensions (nreverse (dimensions mm)))))
    (loop :for i :from 0 :below (rows mm) :do
	 (loop :for j :from 0 :below (cols mm) :do
	      (setf (mref rez j i) (mref mm i j))))
    rez))

(defmethod transpose ((mm cons))
  (apply #'mapcar #'list mm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rows ((mm <matrix>))
  (array-dimension (matrix-data mm) 0))

(defmethod rows ((a array))
  "@b(Описание:) метод @b(rows) возвращает количество строк в массиве @b(a)."
  (assert (= (array-rank a) 2))
  (array-dimension a 0))

(defmethod rows ((2d-list cons))
  (length 2d-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cols
(defmethod cols ((a array))
    "@b(Описание:) метод @b(rows) возвращает количество столбцов в массиве @b(a)."
  (assert (= (array-rank a) 2))
  (array-dimension a 1))

(defmethod cols ((2d-list cons))
  (math/stat:min-value (mapcar #'length 2d-list)))

(defmethod cols ((mm <matrix>))
  (array-dimension (matrix-data mm) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; row
(defmethod row ((mm <matrix>) row)
  (let ((data (matrix-data mm)))
    (loop :for c :from 0 :below (cols mm)
	  :collect (aref data row c))))

(defmethod row ((row integer) (a array))
  "@b(Описание:) метод @b(row) возвращает строку @b(row) из масства @b(a).
Строка возвращается в виде вектора vector.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((arr (make-array '(5 2)
			 :initial-contents '((0 1)
					     (2 3)
					     (4 5)
					     (6 7)
					     (8 9)))))
   (row  0 arr)		    ;=> #(0 1)
   (row  2 arr)	            ;=> #(4 5)
   (row  4 arr)		    ;=> #(8 9))
@end(code)
"
  (when (/= (array-rank a) 2) (error 'row-operation-not-appicable))
  (assert (< -1 row (array-dimension a 0)))
  (apply #'vector (loop :for j :from 0 :below (array-dimension a 1)
			:collect
			(aref a row j))))


(defmethod row (row (2d-list cons))
  (nth row 2d-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; dimensions

(defmethod dimensions ((2d-list cons))
  (list (rows 2d-list) (cols 2d-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; col

(defmethod col ((mm <matrix>) col)
  (let ((data (matrix-data mm)))
    (loop :for r :from 0 :below (rows mm)
	  :collect (aref data r col))))

(defmethod col (col (2d-list cons))
  (mapcar #'(lambda (el) (nth col el)) 2d-list))

(defmethod col ((col integer) (a array))
  "@b(Описание:) метод @b(col) возвращает столбец @b(col) из масства @b(a).
Столбец возвращается в виде вектора (vector).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((arr (make-array '(5 2)
			 :initial-contents '((0 1)
					     (2 3)
					     (4 5)
					     (6 7)
					     (8 9)))))
    (col  0 arr)			;=> #(0 2 4 6 8)
    (col  1 arr)			;=> #(1 3 5 7 9)
    )
@end(code)
"
  (when (/= (array-rank a) 2) (error 'col-operation-not-appicable))
  (assert (< -1 col (array-dimension a 1)))
  (apply #'vector (loop :for i :from 0 :below (array-dimension a 0) :collect
								    (aref a i col))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (setf row)

(defmethod (setf row) (new-value-lst (mm <matrix>) row )
  (let ((data (matrix-data mm))
	(ll new-value-lst))
    (loop :for c :from 0 :below (cols mm)
       :do (setf (aref data row c) (car ll)
		 ll (cdr ll)))
    mm))

(defmethod (setf row) ((new-row cons) (a array) row)
  (assert (= (array-rank a) 2))
  (assert (< -1 row (rows a)))
  (assert (= (cols a) (length new-row)))
  (let ((ll new-row))
    (loop :for c :from 0 :below (cols a)
       :do (setf (aref a row c) (car ll)
		 ll (cdr ll)))
    a))

(defmethod (setf row) ((new-val number) (a array) row)
  (assert (= (array-rank a) 2))
  (assert (< -1 row (rows a)))
  (loop :for c :from 0 :below (cols a) :do (setf (aref a row c) new-val))
  a)

(defmethod (setf row) ((new-row vector) (a array) row)
  (assert (= (array-rank a) 2))
  (assert (< -1 row (rows a)))
  (assert (= (cols a) (length new-row)))
  (loop :for c :from 0 :below (cols a) :do (setf (aref a row c) (svref new-row c)))
  a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (setf col)

(defmethod (setf col) (new-value-lst (mm <matrix>) col )
  (let ((data (matrix-data mm))
	(ll new-value-lst))
    (loop :for r :from 0 :below (rows mm)
       :do (setf (aref data r col) (car ll)
		 ll (cdr ll)))
    mm))

(defmethod (setf col) ((new-col cons) (a array) col )
  (assert (= (array-rank a) 2))
  (assert (< -1 col (cols a)))
  (assert (= (rows a) (length new-col)))
  (let ((ll new-col))
    (loop :for r :from 0 :below (rows a)
       :do (setf (aref a r col) (car ll)
		 ll (cdr ll)))
    a))

(defmethod (setf col) ((new-col vector) (a array) col )
  (assert (= (array-rank a) 2))
  (assert (< -1 col (cols a)))
  (assert (= (rows a) (length new-col)))
  (loop :for r :from 0 :below (rows a) :do (setf (aref a r col) (svref new-col r)))
  a)

(defmethod (setf col) ((new-val number) (a array) col )
  (assert (= (array-rank a) 2))
  (assert (< -1 col (cols a)))
  (loop :for r :from 0 :below (rows a) :do (setf (aref a r col) new-val))
  a)
