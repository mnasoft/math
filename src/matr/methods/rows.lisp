;;;; ./src/matr/methods/rows.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; rows

(defmethod rows ((mm <matrix>))
  (array-dimension (matrix-data mm) 0))

(defmethod rows ((a array))
  "@b(Описание:) метод @b(rows) возвращает количество строк в массиве @b(a)."  
  (assert (= (array-rank a) 2))
  (array-dimension a 0))

(defmethod rows ((2d-list cons))
  (length 2d-list))
