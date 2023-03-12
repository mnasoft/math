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
  "@b(Описание:) функция @b(make-1d-list) возвращает 2d-list размером
 @b(size)*@b(size), состоящий из случайных целых чисел.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-2d-list 1) => ((9))
 (make-2d-list 2) => ((9 4) (12 1))
 (make-2d-list 5) '((2  12  5  4  8)
                        (12 13  0  3 14)
                        (10  9 13  2  3)
                        ( 6  6  0  6  0)
                        ( 1  0  3 10  6))
@end(code)
"
  (loop :for i :from 0 :below size
        :collect
        (loop :for j :from 0 :below size
              :collect (random arg))))

(defun make-ls-system (m vec)
  "@b(Описание:) функция @b(make-ls-system) возвращает 2d-list,
  представляющий из себя расширенную матрицу (матрицу с правыми
  частями), являющуюся представлением системы линейных уравнений, при
  решении которой результатом будет @b(vec).
 
 @b(Пример использования:)
@begin[lang=lisp](code)
 (let* ((n 1)
        (m (make-2d-list n))
        (v (make-1d-list n)))
   (values (make-ls-system m v) v))
  => ((2 14)), (7)
 (let* ((n 3)
        (m (make-2d-list n))
        (v (make-1d-list n)))
   (values (make-ls-system m v) v))
  => ((14 1 1 133)
      (0 2 12 84)
      (4 3 2 50)),
      (9 0 7)
@end(code)"
  (assert (= (length m) (length vec)))
  (math/matr:append-col 
   (mapcar
    #'(lambda(el)
        (apply #'+ (mapcar #'(lambda(v a) (* v a)) vec el)))
    m)
   m))
