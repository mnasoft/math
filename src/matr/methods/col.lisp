;;;; ./src/matr/methods/col.lisp
(in-package :math/matr)

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
  (apply #'vector
         (loop :for i :from 0 :below (array-dimension a 0)
               :collect (aref a i col))))

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
