;;;; ./src/rnd/rnd-doc.lisp
(in-package #:math/rnd)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))


(make-doc
  #'MATH/RND:MAKE-1D-LIST 'function
  "@b(Описание:) функция @b(make-1d-list) возвращает 1d-list длиной
 @b(size), состоящий из случайных целых чисел в диапазоне от 0 до
 @b(arg).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-1d-list 1) => (8)
 (make-1d-list 2) => (10 7)
 (make-1d-list 5) => (8 13 5 6 11) 
@end(code)
")

(make-doc
  #'MATH/RND:MAKE-2D-LIST 'function
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
")

(make-doc
  #'MATH/RND:MAKE-LS-SYSTEM 'function
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
@end(code)")
