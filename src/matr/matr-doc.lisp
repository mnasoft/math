
(in-package #:MATH/MATR)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))

(make-doc
  (find-method #'MATH/MATR:ROTATE-Y NIL '(NUMBER))
  t
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
  преобразования, которая вращает систему координат на угол β вокруг
  оси y.")

(make-doc
  (find-method #'MATH/MATR:MATR-EVAL-* NIL '(MATH/MATR:<MATRIX>))
  t
  "Мутная функция и непонятно как ее использовать и где?")

(make-doc
  (find-method #'MATH/MATR:ANTI-DIAGONAL NIL '(MATH/MATR:<MATRIX>))
  t
  "@b(Пример использования:)
@begin[lang=lisp](code)
 (defparameter *mm* 
  (make-instance '<matrix> 
   :initial-contents '((1d0 2d0 3d0) 
                       (4d0 5d0 6d0) 
                       (7d0 8d0 9d0))))
 =>
 Matr 3х3
 [ 1.0d0     2.0d0     3.0d0    ]
 [ 4.0d0     5.0d0     6.0d0    ]
 [ 7.0d0     8.0d0     9.0d0    ]

  (anti-diagonal  *mm*) => (3.0d0 5.0d0 7.0d0)
  @end(code)")

(make-doc
  (find-method #'MATH/MATR:ROTATE-Z NIL '(NUMBER))
  t
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая вращает систему координат на угол γ вокруг оси
z.")

(make-doc
  (find-method #'MATH/MATR:ROTATE-AROUND NIL '(CONS CONS T))
  t
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая вращает протсранство вокруг оси, заданной
точками point-1 и point-2")


