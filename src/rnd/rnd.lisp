;;;; ./src/rnd/rnd.lisp

(defpackage :math/rnd
  (:use #:cl) 
  (:export make-1d-list 
           make-2d-list 
	   make-ls-system)
  (:documentation
   "@b(Описание:) пакет @b(math/rnd) содержит функции, предназначенные
   для создания случайных одномерных (1d-list) и двумерных (2d-list)
   списков, а также систем линейных уравнений со сручайным наперед
   заданным решением."))

(in-package :math/rnd)

(defun make-1d-list (size &optional (arg 15))
  "@b(Описание:) функция @b(make-1d-list) возвращает 1d-list длиной
 @b(size), состоящий из случайных целых чисел в диапазоне от 0 до
 @b(arg).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-1d-list 1) => (8)
 (make-1d-list 2) => (10 7)
 (make-1d-list 5) => (8 13 5 6 11) 
@end(code)
"
  (loop :for i :from 0 :below size
        :collect (random arg)))

(defun make-2d-list (size &optional (arg 15))
  "@b(Описание:) функция @b(make-2d-list) возвращает 2d-list размером
@b(size)*@b(size), состоящий из случайных целых чисел в диапазоне
от 0 до @b(arg - 1).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-2d-list 1) => ((9))
 (make-2d-list 2) => ((9 4) (12 1))
 (make-2d-list 3) => ((8 2 0) (5 13 11) (1 7 3))
@end(code)
"
  (loop :for i :from 0 :below size
        :collect
        (loop :for j :from 0 :below size
              :collect (random arg))))

(defun make-ls-system (m vec)
  "@b(Описание:) функция @b(make-ls-system) возвращает 2d-list,
представляющий расширенную матрицу (матрицу с правыми частями) для
системы линейных уравнений Ax = b, где A — матрица @b(m), b — вектор
@b(vec). Результат можно подать на вход модулю solve-x.

 @b(Переменые:)
@begin(list)
 @item(m   - матрица коэффициентов, список списков.)
 @item(vec - вектор правой части, список чисел.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let* ((m '((2 1) (1 3)))
        (v '(8 7)))
   (make-ls-system m v))
 => ((10 1 8) (15 3 7)) ; (2*8+1*7=23, 1*8+3*7=29) корректный вид
@end(code)"
  (assert (= (length m) (length vec)))
  (math/matr:append-col 
   (mapcar
    #'(lambda(el)
        (apply #'+ (mapcar #'(lambda(v a) (* v a)) vec el)))
    m)
   m))
