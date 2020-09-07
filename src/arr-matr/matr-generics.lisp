;;;; matr-generics.lisp

(in-package :math/arr-matr)


(export 'convert-to-triangular)

(defgeneric convert-to-triangular (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(convert-to-triangular) 
выполняет приведение  матрицы @b(matrix) к треугольному виду,
для решения системы ЛУ методом Гаусса."))

(export 'solve-linear-system-gauss)

(defgeneric solve-linear-system-gauss (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(solve-linear-system-gauss)
возвращает матрицу, содержащую корни решения системы линейных уравений.

 Решение системы линейных уравнений выполняется методом Гаусса.
"))

(export 'add)

(defgeneric add (a b)
  (:documentation "@b(Описание:) обобщенная_функция @b(multiply)
выполняет сложение аргументов @b(a) и @b(b)."))

(export 'multiply)

(defgeneric multiply (a b)
    (:documentation "@b(Описание:) обобщенная_функция @b(multiply) 
выполняет выполняет перемножение аргументов @b(a) и @b(b)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric matr-eval-* (matrix) (:documentation "Matr"))

(defgeneric matr-equal* (matrix1 matrix2 &key test) (:documentation "Проверка матриц на равенство"))

;;;; (setf *print-case* :downcase)
;;;; (setf *print-case* :upcase)

(export 'approx-by-points)

(defgeneric approx-by-points (pnt d-pnt points values &key w-func)
  (:documentation
   "Вычисляет функцию, заданную точками points и значениями values
 в точке pnt, используя размер влияния, заданный параметром d-pnt.
 "))

(export 'refine-approximation-values)

(defgeneric refine-approximation-values (points values base-dist-s &key w-func delta iterations)
  (:documentation
"Выполняет поиск массива значений такого, что:
 - при сглаживании функцией w-func ;
 - с размером сглаживания dx0 ;
 - в точках points (аргументы функции) ;
 - сумма отклонений сглаженных значений от значений, заданных в узлах не превысит значения delta."))
