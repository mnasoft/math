;;;; matr-generics.lisp

(in-package :math/arr-matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric matr-eval-* (matrix) (:documentation "Matr"))

(defgeneric matr-equal* (matrix1 matrix2 &key test) (:documentation "Проверка матриц на равенство"))

;;;; (setf *print-case* :downcase)
;;;; (setf *print-case* :upcase)

(export 'smooth-by-points)

(defgeneric smooth-by-points (pnt d-pnt points values &key w-func)
  (:documentation
   "Вычисляет функцию, заданную точками points и значениями values
 в точке pnt, используя размер влияния, заданный параметром d-pnt.
 "))

(export 'refine-smoothing-by-points)

(defgeneric refine-smoothing-by-points (points values base-dist-s &key w-func delta iterations)
  (:documentation
"Выполняет поиск массива значений такого, что:
 - при сглаживании функцией w-func ;
 - с размером сглаживания dx0 ;
 - в точках points (аргументы функции) ;
 - сумма отклонений сглаженных значений от значений, заданных в узлах не превысит значения delta."))
