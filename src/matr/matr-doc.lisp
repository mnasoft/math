
(in-package #:MATH/MATR)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))

(make-doc
  (find-method #'MATH/MATR:TRANSPOSE NIL '(MATH/MATR:<MATRIX>))
  t
  "@b(Описание:) метод @b(transpose) возвращает матрицу типа <matrix>,
   являющуютя результатом транспонирования матрицы @b(mm).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (transpose (matr-new 2 3 '(1 2 3
                            4 5 6)))
  => Matr 3х2
     [ 1         4        ]
     [ 2         5        ]
     [ 3         6        ]
@end(code)")

(make-doc
  (find-method #'MATH/MATR:TRANSPOSE NIL '(CONS))
  t
  "Выполняет транспонирование")

(make-doc
  (find-method #'MATH/MATR:ROWS NIL '(ARRAY))
  t
  "@b(Описание:) метод @b(rows) возвращает количество строк в массиве @b(a).")

(make-doc
  (find-method #'MATH/MATR:ADD NIL '(MATH/MATR:<MATRIX> MATH/MATR:<MATRIX>))
  t
  "@b(Описание:) метод @b(add) возвращает матрицу типа <matrix>,
являющуюся результатом сложения матриц a и b.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (add (matr-new 2 2 '(1 2 
                      3 4)) 
      (matr-new 2 2 '(1 2 
                      3 4)))
  => Matr 2х2
     [ 2         4        ]
     [ 6         8        ]
 (add (matr-new 2 2 '(1 2 3 4)) (matr-new 2 2 '(4 3 2 1)))
  => Matr 2х2
     [ 5         5        ]
     [ 5         5        ]
@end(code)
")

(make-doc
  (find-method #'MATH/MATR:ROTATE-V NIL '(NUMBER CONS))
  t
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая вращает систему координат на угол α вокруг
оси, заданной вектором v. Вектор v должен быть нормализованным (иметь
единичную длину).")

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

(make-doc
  (find-method #'(SETF MATH/MATR:MAIN-DIAGONAL) NIL '(T MATH/MATR:<MATRIX>))
  t
  "@b(Пример использования:)
@begin[lang=lisp](code)
 (defparameter *mm* 
   (make-instance '<matrix> 
      :initial-contents '(( 1d0  2d0  3d0) 
                          ( 4d0  5d0  6d0) 
                          ( 7d0  8d0  9d0) 
                          (10d0 11d0 12d0))))
 *mm* =>  Matr 4х3
          [ 1.0d0     2.0d0     3.0d0    ]
          [ 4.0d0     5.0d0     6.0d0    ]
          [ 7.0d0     8.0d0     9.0d0    ]
          [ 10.0d0    11.0d0    12.0d0   ]

 (setf (main-diagonal *mm*) '(11d0 22d0 33d0)) 
 *mm* => Matr 4х3
        [ 11.0d0    2.0d0     3.0d0    ]
        [ 4.0d0     22.0d0    6.0d0    ]
        [ 7.0d0     8.0d0     33.0d0   ]
        [ 10.0d0    11.0d0    12.0d0   ]
@end(code)")

(make-doc
  (find-method #'(SETF MATH/MATR:MAIN-DIAGONAL) NIL '(NUMBER MATH/MATR:<MATRIX>))
  t
  "@b(Пример использования:)
@begin[lang=lisp](code)
 (defparameter *mm* 
   (make-instance '<matrix> 
      :initial-contents '(( 1d0  2d0  3d0) 
                          ( 4d0  5d0  6d0) 
                          ( 7d0  8d0  9d0) 
                          (10d0 11d0 12d0))))
 *mm* =>  Matr 4х3
          [ 1.0d0     2.0d0     3.0d0    ]
          [ 4.0d0     5.0d0     6.0d0    ]
          [ 7.0d0     8.0d0     9.0d0    ]
          [ 10.0d0    11.0d0    12.0d0   ]

 (setf (main-diagonal *mm*) 11d0) 
  Matr 4х3
  [ 11.0d0     2.0d0    3.0d0   ]
  [ 4.0d0     11.0d0    6.0d0   ]
  [ 7.0d0      8.0d0   11.0d0   ]
  [ 10.0d0    11.0d0   12.0d0   ]

 *mm* => Matr 4х3
  [ 11.0d0    2.0d0     3.0d0    ]
  [ 4.0d0     11.0d0    6.0d0    ]
  [ 7.0d0     8.0d0     11.0d0   ]
  [ 10.0d0    11.0d0    12.0d0   ]
@end(code)")
