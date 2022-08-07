;;;; ./src/list-matr/list-matr-doc.lisp

(in-package :math/matr)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



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
  #'MATH/MATR:MAX-COL-NOT-NIL-VALUE 'function
  "@b(Описание:) функция @b(max-col-not-nil-value) вычисляет среднее 
значение по столбцам матрицы (списка списков).")

(make-doc
  #'MATH/MATR:AVERAGE-NOT-NIL-VALUE 'function
  "@b(Описание:) функция @b(average-not-nil-value) вычисляет среднее значение
по элементам матрицы (списка списков) с исключением nil-элементов.")

(make-doc
  #'MATH/MATR:LM-PRINT 'function
  "@b(Описание:) функция @b(lm-print) красивый вывод
матрицы (списка списков).")

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
  #'MATH/MATR:AVERAGE-COL-VALUE 'function
  "@b(Описание:) функция @b(average-col-value) вычисляет среднее значение
по столбцам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-col-value '((3.0 2.1 4.5)
                     (2.0 2.5 3.2))) => (2.5 2.3 3.85)
@end(code)")

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
  #'MATH/MATR:AVERAGE-ROW-NOT-NIL-VALUE 'function
  "@b(Описание:) функция @b(average-row-not-nil-value) вычисляет среднее значение
по элементам матрицы (списка списков).")

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
  #'MATH/MATR:ROWS 'function
  "@b(Описание:) функция @b(rows) возвращает
количество строк в матрице, заданной списком 2d-list

 @b(Пример использования:)
@begin[lang=lisp](code)
 (rows '((0 1 2) (3 4 5)))  =>2
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
  #'MATH/MATR:AVERAGE-VALUE 'function
  "@b(Описание:) функция @b(average-value) вычисляет среднее значение по
элементам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-value '((1.0 1.5 2.0)
                  (2.0 2.5 3.0))) => 2.0
@end(code)")

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
  #'MATH/MATR:LV-PRINT 'function
  "@b(Описание:) функция @b(lv-print) красивый вывод
 вектора (списка).")
