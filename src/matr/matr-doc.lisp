
(in-package #:MATH/MATR)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



(make-doc
  (find-package 'MATH/MATR) t
  "@b(Описание:) пакет @b(math/matr) определяет некоторые операции
 над матрицами, представленными 2d-list, (списком состоящим из
 списков)")

(make-doc
  (macro-function 'MATH/MATR::MAKE-DOC) t
  NIL)

(make-doc
  #'MATH/MATR:LV-PRINT 'function
  "@b(Описание:) функция @b(lv-print) красивый вывод
 вектора (списка).")

(make-doc
  #'MATH/MATR:NORMALIZE 'function
  "@b(Описание:) функция @b(normalize) возвращает нормализованный вектор.
Длина нормализованного вектора равна 1.0.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (normalize '(1 2 3)) => (0.26726124 0.5345225 0.8017837)
 (normalize '(2 -3))  => (0.5547002 -0.8320503)
@end(code)
")

(make-doc
  #'MATH/MATR:GET-LAST-COL 'function
  "@b(Описание:) функция @b(get-last-col) возвращает последний столбец матрицы, 
представленной в виде списка.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (get-last-col
 '((1  2  3  6)
   (2  3  4  9) 
   (3  4  5 12))) =>(6 9 12) 
@end(code)
")

(make-doc
  #'MATH/MATR:MAX-ROW-NOT-NIL-VALUE 'function
  "@b(Описание:) функция @b(max-row-not-nil-value) вычисляет максимальные 
значения по строкам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (max-row-not-nil-value '((1.0 2.1 1.5 2.0)
                          (2.0 2.5 3.2 3.0))) => (2.1 3.2)
@end(code)
")

(make-doc
  #'MATH/MATR:MATR-NEW 'function
  "Примечание:
 (matr-new 3 4 '(1 2 3 4 5 6 7 8 9 10)) ")

(make-doc
  #'MATH/MATR:AVERAGE-ROW-NOT-NIL-VALUE 'function
  "@b(Описание:) функция @b(average-row-not-nil-value) вычисляет среднее значение
по элементам матрицы (списка списков).")

(make-doc
  #'MATH/MATR:AVERAGE-COL-VALUE 'function
  "@b(Описание:) функция @b(average-col-value) вычисляет среднее значение
по столбцам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-col-value '((3.0 2.1 4.5)
                     (2.0 2.5 3.2))) => (2.5 2.3 3.85)
@end(code)")

(make-doc
  #'MATH/MATR:UNITE-ROWS 'function
  "@b(Описание:) функция @b(unite-rows) объединяет строки матрицы
(список списков) в вектор (список).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (unite-rows '((1 2 3)
               (4 5 6)))
 =>(1 2 3 4 5 6)
@end(code)
")

(make-doc
  #'MATH/MATR:PREPEND-ROW 'function
  "@b(Описание:) функция @b(prepend-row)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (prepend-row '(10 11 12) 
              '((1 2 3)
                (4 5 6)
                (7 8 9))) =>((10 11 12) 
                             ( 1  2  3) 
                             ( 4  5  6)
                             ( 7  8  9))
 (math:prepend-row '(10 11 )
                   '((1 2 3)
                     (4 5 6) 
                     (7 8 9))) =>((10 11 NIL) 
                                  (1 2 3) 
                                  (4 5 6) 
                                  (7 8 9))
 (math:prepend-row '(10 11 12 13)
                   '((1 2 3)
                     (4 5 6)
                     (7 8 9)))  =>((10 11 12)
                                   ( 1  2  3) 
                                   ( 4  5  6) 
                                   ( 7  8  9))
@end(code)
")

(make-doc
  #'MATH/MATR:APPEND-COL 'function
  "@b(Описание:) функция @b(append-col) добавляет к матрице,
представленной списком 2d-list, столбец (список) c-lst.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (math:append-col '(10 11 12)
                  '((1 2 3)
                    (4 5 6)
                    (7 8 9))) => ((1 2 3 10) 
                                  (4 5 6 11)
                                  (7 8 9 12))
@end(code)
")

(make-doc
  #'MATH/MATR:DETACH-LAST-COL 'function
  "@b(Описание:) функция @b(detach-last-col) возвращает матрицу, 
представленную в виде списка, образованную удалением последнего столбца 
(последнего элемента каждой строки).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (detach-last-col
  '((1 2 3  6)
    (2 3 4  9) 
    (3 4 5 12))) => ((1 2 3)
                     (2 3 4)
                     (3 4 5))
@end(code)
")

(make-doc
  #'MATH/MATR:MAX-COL-NOT-NIL-VALUE 'function
  "@b(Описание:) функция @b(max-col-not-nil-value) вычисляет среднее 
значение по столбцам матрицы (списка списков).")

(make-doc
  #'MATH/MATR:APPEND-ROW 'function
  "@b(Описание:) функция @b(append-row) добавляет вектор
(список) r-lst к матрице 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (math:append-row '(10 11 12)
                  '((1 2 3)
                    (4 5 6)
                    (7 8 9)))  =>((1 2 3)
                                  (4 5 6)
                                  (7 8 9)
                                  (10 11 12))

 (math:append-row '(10 11 )
                  '((1 2 3)
                    (4 5 6)
                    (7 8 9)))
 =>((1 2 3)
    (4 5 6)
    (7 8 9)
    (10 11 NIL))

 (math:append-row '(10 11 12 13)
                  '((1 2 3)
                    (4 5 6)
                    (7 8 9))) =>(( 1  2  3)
                                 ( 4  5  6)
                                 ( 7  8  9)
                                 (10 11 12))
@end(code)
")

(make-doc
  #'MATH/MATR:PREPEND-COL 'function
  "@b(Описание:) функция @b(prepend-col) добавляет вектор
(список) c-lst к матрице 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (prepend-col '(10 11 12)
              '((1  2  3)
                (4  5  6)
                (7  8  9))) => ((10 1 2 3)
                                (11 4 5 6)
                                (12 7 8 9))

@end(code)
")

(make-doc
  #'MATH/MATR:AVERAGE-NOT-NIL-VALUE 'function
  "@b(Описание:) функция @b(average-not-nil-value) вычисляет среднее значение
по элементам матрицы (списка списков) с исключением nil-элементов.")

(make-doc
  #'MATH/MATR:AVERAGE-VALUE 'function
  "@b(Описание:) функция @b(average-value) вычисляет среднее значение по
элементам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-value '((1.0 1.5 2.0)
                  (2.0 2.5 3.0))) => 2.0
@end(code)")

(make-doc
  #'MATH/MATR:MAKE 'function
  "@b(Описание:) функция @b(make) генерирует матрицу 
(список списков) из вектора (списока).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (math:make 2 3 '(0 1 2 3 4 5 6 7 8 9 10 11 12))
 => ((0 1 2) 
     (3 4 5))
 (math:make 2 3 'nil)
 => ((NIL NIL NIL) 
     (NIL NIL NIL))
@end(code)
")

(make-doc
  #'MATH/MATR:LM-PRINT 'function
  "@b(Описание:) функция @b(lm-print) красивый вывод
матрицы (списка списков).")

(make-doc
  #'MATH/MATR:PREPEND-ROWS 'function
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (prepend-rows
 '((10 20 30)
   (11 22 33))
 '((11 12 13)
   (12 13 14)
   (13 14 15)))
@end(code)
")

(make-doc
  #'MATH/MATR:AVERAGE-ROW-VALUE 'function
  "@b(Описание:) функция @b(average-row-value) вычисляет 
средние значения в строках матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-row-value '((1.0 1.5 2.0)
                      (2.0 2.5 3.0))) => (1.5 2.5)
@end(code)
")

(make-doc
  #'MATH/MATR:AVERAGE-COL-NOT-NIL-VALUE 'function
  "@b(Описание:) функция @b(average-col-not-nil-value) вычисляет среднее 
значение по элементам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-col-not-nil-value '((nil 2.1 4.5)
                             (2.0 nil 3.2))) => (2.0 2.1 3.85)
  
@end(code)
")

(make-doc
  #'MATH/MATR:MAKE-VECTOR-N 'function
  "Пример использования:
 (make-vector-n 1.5 3) => #(1.5 1.5 1.5)")

(make-doc
  #'MATH/MATR:DIMENSIONS 'function
  "@b(Описание:) функция @b(cols) возвращает 
количество столбцов в матрице, представленной списком 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dimensions '((0 1 2)
               (3 4 5)))    => (2 3)
 (dimensions '((0 1 2 10) 
               (3 4 5   ))) => (2 3)
@end(code)
")

(make-doc
  #'MATH/MATR:MATR-EVAL-* 'function
  "Matr")

(make-doc
  #'MATH/MATR:ROWS 'function
  "@b(Описание:) функция @b(rows) возвращает
количество строк в матрице, заданной списком 2d-list

 @b(Пример использования:)
@begin[lang=lisp](code)
 (rows '((0 1 2) (3 4 5)))  =>2
@end(code)
")

(make-doc
  #'MATH/MATR:TRANSPOSE 'function
  "@b(Описание:) функция @b(transpose) выполняет транспонирование матрицы,
представленной списком 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (transpose '((1 2 3)
              (4 5 6)))
 =>((1 4) 
    (2 5) 
    (3 6))
@end(code)
")

(make-doc
  #'MATH/MATR:COL 'function
  "@b(Описание:) функция @b(col) возвращает столбец
@b(col) матрицы, представленной списком @b(2d-list).

 @b(Примечание:) Нумерация столбцов начинается с 0

 @b(Пример использования:)
@begin[lang=lisp](code)
 (col 1 '((0 1 2) 
          (3 4 5)))
 =>(1 4)
@end(code)")

(make-doc
  #'MATH/MATR:COLS 'function
  "@b(Описание:) функция @b(cols) возвращает 
количество столбцов в матрице, представленной списком 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (cols '((0 1 2)
         (3 4 5)))  =>3
 (cols '((0 1 2 10) 
         (3 4 5)))  =>3
@end(code)
")

(make-doc
  #'MATH/MATR:ROW 'function
  "@b(Описание:) функция @b(row) возвращает строку row матрицы,
представленной списком 2d-list.

 @b(Примечание:) Нумерация строк начинается с 0.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (row 1 '((0 1 2) 
          (3 4 5)))
 =>(3 4 5)
@end(code)
")

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
  (find-method #'MATH/MATR:TRANSFORM NIL '(CONS MATH/MATR:<MATRIX>))
  t
  "@b(Описание:) метод @b(transform) возвращает координаты точки
  @b(point), преобразованные с помощью матрицы matrix.")

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
  (find-method #'MATH/MATR:MOVE-XYZ NIL '(NUMBER NUMBER NUMBER))
  t
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая перемещает систему координат на (dx dy dz).
")

(make-doc
  (find-method #'MATH/MATR:ROW NIL '(INTEGER ARRAY))
  t
  "@b(Описание:) метод @b(row) возвращает строку @b(row) из масства @b(a).
Строка возвращается в виде вектора vector.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((arr (make-array '(5 2)
			 :initial-contents '((0 1)
					     (2 3)
					     (4 5)
					     (6 7)
					     (8 9)))))
   (row  0 arr)		    ;=> #(0 1)
   (row  2 arr)	            ;=> #(4 5)
   (row  4 arr)		    ;=> #(8 9))
@end(code)
")

(make-doc
  (find-method #'MATH/MATR:MULTIPLY NIL '(MATH/MATR:<MATRIX> MATH/MATR:<MATRIX>))
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
  (find-method #'MATH/MATR:ROTATE-X NIL '(NUMBER))
  t
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
  преобразования, которая вращает систему координат на угол α вокруг
  оси x.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn (defparameter *p* (make-instance 'math/matr:<matrix> :dimensions '(1 4)))
        (setf (math/matr:row *p* 0) '(10.0 20.0 30.0 1.0))
        (math/matr:multiply *p* (rotate-y (dtr 90.0))))
@end(code)
")

(make-doc
  (find-method #'MATH/MATR:COLS NIL '(ARRAY))
  t
  "@b(Описание:) метод @b(rows) возвращает количество столбцов в массиве @b(a).")

(make-doc
  (find-method #'MATH/MATR:MOVE-V NIL '(CONS))
  t
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая перемещает систему координат в направлении
вектора v.")

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
  (find-method #'MATH/MATR:COL NIL '(INTEGER ARRAY))
  t
  "@b(Описание:) метод @b(col) возвращает столбец @b(col) из масства @b(a).
Столбец возвращается в виде вектора (vector).

 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((arr (make-array '(5 2)
			 :initial-contents '((0 1)
					     (2 3)
					     (4 5)
					     (6 7)
					     (8 9)))))
    (col  0 arr)			;=> #(0 2 4 6 8)
    (col  1 arr)			;=> #(1 3 5 7 9)
    )
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
