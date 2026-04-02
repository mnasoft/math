;;;; package.lisp

(defpackage :math 
  (:use #:cl)
  (:export mult-matr-vect)
  (:documentation
   "@b(Описание:) Пакет @b(:math) является верхнеуровневым пакетом
проекта и реэкспортирует общие утилиты, не входящие в специализированные
подпакеты.

Для использования отдельных подсистем загружайте соответствующий пакет:
@b(:math/core), @b(:math/stat), @b(:math/matr), @b(:math/appr) и др.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (ql:quickload :math)
@end(code)"))

(in-package :math)

(defun mult-matr-vect (matr vect)
  "@b(Описание:) функция @b(mult-matr-vect) возвращает вектор, являющийся
результатом умножения квадратной матрицы @b(matr) на вектор @b(vect).
Размерность результирующего вектора совпадает с размерностью @b(vect).

 @b(Переменые:)
@begin(list)
 @item(matr - двумерный массив (матрица) размером n×n;)
 @item(vect - одномерный массив (вектор) размером n.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mult-matr-vect
  (make-array '(2 2) :initial-contents '((1.0d0 2.0d0) (3.0d0 4.0d0)))
  (make-array 2 :initial-contents '(1.0d0 1.0d0)))
 => #(3.0d0 7.0d0)

 (mult-matr-vect
  (make-array '(3 3) :initial-contents '((1.0d0 0.0d0 0.0d0)
                                         (0.0d0 2.0d0 0.0d0)
                                         (0.0d0 0.0d0 3.0d0)))
  (make-array 3 :initial-contents '(2.0d0 3.0d0 4.0d0)))
 => #(2.0d0 6.0d0 12.0d0)
@end(code)"
  (let* ((n (array-dimension vect 0))
	 (vect-rez (make-array n :initial-element 0.0d0)))
    (do ((i 0 (1+ i)))
	((= i n) (values vect-rez matr vect ))
      (do ((j 0 (1+ j))
	   (summ 0.0d0))
	  ((= j n) (setf (aref vect-rez i) summ))
	(setf summ (+ summ (* (aref matr i j )
			      (aref vect j))))))))

