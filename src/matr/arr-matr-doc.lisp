;;;; ./src/arr-matr/arr-matr-doc.lisp
(in-package :math/matr)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))

(make-doc
  (find-method #'INITIALIZE-INSTANCE NIL '(MATH/MATR:<MATRIX>))
  t
  NIL)

(make-doc
  #'MATH/MATR:MATR-NEW 'function
  "Примечание:
 (matr-new 3 4 '(1 2 3 4 5 6 7 8 9 10)) ")

(make-doc
  #'MATH/MATR:TRANSPOSE 'function
  "@b(Описание:) обобщенная_функция @b(transpose) возвращает, являющуютя
результатом транспонирования матрицы @b(mm).")

(make-doc
  #'MATH/MATR:MULTIPLY 'function
  NIL)

(make-doc
  #'MATH/MATR:ROW 'function
  NIL)

(make-doc
  #'MATH/MATR:MATRIX->2D-LIST 'function
  NIL)

(make-doc
  #'MATH/MATR:MATRIX-DATA 'function
  NIL)

(make-doc
  #'MATH/MATR:MREF 'function
  NIL)

(make-doc
  #'MATH/MATR:ROWS 'function
  NIL)

(make-doc
  #'MATH/MATR:COPY 'function
  NIL)

(make-doc
  #'MATH/MATR:MAIN-DIAGONAL 'function
  NIL)

(make-doc
  #'MATH/MATR:SWAP-COLS* 'function
  NIL)

(make-doc
  #'MATH/MATR:SWAP-ROWS 'function
  NIL)

(make-doc
  #'MATH/MATR:EQUIVALENT 'function
  NIL)

(make-doc
  #'MATH/MATR:DIMENSIONS 'function
  NIL)

(make-doc
  #'MATH/MATR:ANTI-DIAGONAL 'function
  NIL)

(make-doc
  #'MATH/MATR:MATR-EVAL-* 'function
  "Matr")

(make-doc
  #'MATH/MATR:ADD 'function
  NIL)

(make-doc
  #'MATH/MATR:SWAP-COLS 'function
  NIL)

(make-doc
  #'MATH/MATR:SQUAREP 'function
  NIL)

(make-doc
  #'MATH/MATR:COLS 'function
  NIL)

(make-doc
  #'MATH/MATR:COL 'function
  NIL)

(make-doc
  #'MATH/MATR:SWAP-ROWS* 'function
  NIL)

(make-doc
  (find-class 'MATH/MATR:<MATRIX>) t
  "Представляет матрицу, определенную через массив.

 Создание:
@begin(list)
 @item(при помощи функции matr-new)
 @item( )
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (matr-new 2 3)
 => Matr 2х3
    [ 0.0d0     0.0d0     0.0d0    ]
    [ 0.0d0     0.0d0     0.0d0    ]
 (matr-new 3 2 '(1 2 3 4 5))
 => Matr 3х2
    [ 1         2        ]
    [ 3         4        ]
    [ 5         NIL      ]
@end(code)
")

(make-doc
 (find-slot 'MATH/MATR::DATA 'MATH/MATR:<MATRIX>)
 t
 "Сдержимое матрицы.")

(make-doc
  (find-method #'MATH/MATR:SWAP-ROWS* NIL '(MATH/MATR:<MATRIX> T T))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:COL NIL '(MATH/MATR:<MATRIX> T))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:COLS NIL '(MATH/MATR:<MATRIX>))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:SQUAREP NIL '(MATH/MATR:<MATRIX>))
  t
  "@b(Описание:) метод @b(squarep) возвращает T, если матрица является
  квадратной. В противном случае возвращает nil.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (defparameter *mm* (make-instance '<matrix>   :dimensions '(2 3)))
  (squarep *mm*) => nil

  (defparameter *mm* (make-instance '<matrix>   :dimensions '(3 3)))
  (squarep *mm*) => T
@end(code)
")

(make-doc
  (find-method #'MATH/MATR:SWAP-COLS NIL '(MATH/MATR:<MATRIX> T T))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:ADD NIL '(MATH/MATR:<MATRIX>
                                         MATH/MATR:<MATRIX>))
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
  (find-method #'MATH/MATR:DIMENSIONS NIL '(MATH/MATR:<MATRIX>))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:EQUIVALENT NIL '(MATH/MATR:<MATRIX>
                                                MATH/MATR:<MATRIX>))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:EQUIVALENT NIL '(ARRAY ARRAY))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:EQUIVALENT NIL '(MATH/MATR:<MATRIX> ARRAY))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:EQUIVALENT NIL '(ARRAY MATH/MATR:<MATRIX>))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:SWAP-ROWS NIL '(MATH/MATR:<MATRIX> T T))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:SWAP-COLS* NIL '(MATH/MATR:<MATRIX> T T))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:MAIN-DIAGONAL NIL '(MATH/MATR:<MATRIX>))
  t
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (defparameter *mm* 
   (make-instance '<matrix> :initial-contents 
    '(( 1d0  2d0  3d0) 
      ( 4d0  5d0  6d0) 
      ( 7d0  8d0  9d0) 
      (10d0 11d0 12d0))))
 *mm*  =>  Matr 4х3
           [ 1.0d0     2.0d0     3.0d0    ]
           [ 4.0d0     5.0d0     6.0d0    ]
           [ 7.0d0     8.0d0     9.0d0    ]
           [ 10.0d0    11.0d0    12.0d0   ]

(main-diagonal *mm*) =>  (1.0d0 5.0d0 9.0d0)
@end(code)")

(make-doc
  (find-method #'MATH/MATR:COPY NIL '(MATH/MATR:<MATRIX>))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:ROWS NIL '(MATH/MATR:<MATRIX>))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:MREF NIL '(MATH/MATR:<MATRIX> T T))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:MATRIX->2D-LIST NIL '(MATH/MATR:<MATRIX>))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:ROW NIL '(MATH/MATR:<MATRIX> T))
  t
  NIL)

(make-doc
  (find-method #'MATH/MATR:MULTIPLY NIL '(MATH/MATR:<MATRIX>
                                              MATH/MATR:<MATRIX>))
  t
  "@b(Описание:) метод @b(multiply) возвращает матрицу типа <matrix>,
являющуюся результатом умножения матриц @b(a) и @b(b).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (multiply (matr-new 2 3 '(1.0 2.0 3.0
			   4.0 5.0 6.0))
	   (matr-new 3 2 '(1.0 2.0
			   3.0 4.0
			   5.0 6.0)))
 => Matr 2х2
    [ 22.0 28.0 ]
    [ 49.0 64.0 ]
 (multiply (matr-new 3 2 '(1.0 2.0
			   3.0 4.0
			   5.0 6.0))
	   (matr-new 2 3 '(1.0 2.0 3.0
			   4.0 5.0 6.0)))
 => Matr 3х3
    [ 9.0  12.0 15.0 ]
    [ 19.0 26.0 33.0 ]
    [ 29.0 40.0 51.0 ]
@end(code)
")

(make-doc
  (find-method #'MATH/MATR:MULTIPLY NIL '(NUMBER MATH/MATR:<MATRIX>))
  t
  "@b(Описание:) метод @b(multiply) возвращает матрицу типа (<matrix>),
     являющуюся результатом умножения числа @b(a) и матрицы @b(b).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (multiply 10 
           (matr-new 2 3 '(1.0 2.0 3.0
			   4.0 5.0 6.0)))
 => Matr 2х3
    [ 10.0      20.0      30.0     ]
    [ 40.0      50.0      60.0     ]
@end(code)
")

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
  (find-method #'(SETF MATH/MATR:COL) NIL '(T MATH/MATR:<MATRIX> T))
  t
  NIL)

(make-doc
  (find-method #'(SETF MATH/MATR:ANTI-DIAGONAL) NIL '(T
                                                          MATH/MATR:<MATRIX>))
  t
  NIL)

(make-doc
  (find-method #'(SETF MATH/MATR:MAIN-DIAGONAL) NIL '(T
                                                          MATH/MATR:<MATRIX>))
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
  (find-method #'(SETF MATH/MATR:MAIN-DIAGONAL) NIL '(NUMBER
                                                          MATH/MATR:<MATRIX>))
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

(make-doc
  (find-method #'(SETF MATH/MATR:MREF) NIL '(T MATH/MATR:<MATRIX> T T))
  t
  NIL)

(make-doc
  (find-method #'(SETF MATH/MATR:ROW) NIL '(T MATH/MATR:<MATRIX> T))
  t
  NIL)
