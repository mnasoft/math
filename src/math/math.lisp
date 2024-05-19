;;;; package.lisp

(defpackage :math 
  (:use #:cl) ;;#:math/core 
  (:export mult-matr-vect))

(in-package :math)

(defun mult-matr-vect (matr vect)
  "@b(Описание:) функция @b(mult-matr-vect) возвращает вектор, являющийся
результатом умножения матрицы @b(matr) на вектор @b(vect). Количество
элементов в результирующем векторе равно количеству элементов в
векторе @b(vect).


 @b(Пример использования:)
@begin[lang=lisp](code)
(defparameter *m-test*
  (make-array '(3 4)
	      :initial-contents
	      '((10.0d0 11.0d0  12.0d0  4.0d0)
		(15.0d0 17.0d0  21.0d0  2.0d0)
		(70.0 8.0  10.0 3.0))))
(mult-matr-vect
 *m-test* ; Проверка правильности решения (системы линейных алгебраических уравнений) СЛАУ
 (solve-linear-system-rotation (cl-utilities:copy-array *m-test*)))
@end(code)
"
  (let* ((n (array-dimension vect 0))
	 (vect-rez (make-array n :initial-element 0.0d0)))
    (do ((i 0 (1+ i)))
	((= i n) (values vect-rez matr vect ))
      (do ((j 0 (1+ j))
	   (summ 0.0d0))
	  ((= j n) (setf (aref vect-rez i) summ))
	(setf summ (+ summ (* (aref matr i j )
			      (aref vect j))))))))

