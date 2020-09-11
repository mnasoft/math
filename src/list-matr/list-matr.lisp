;;;; list-matr.lisp

;;; В данном файле определяются некоторые операции над матрицами,
;;; представленными 2d-list, (списком состоящим из списков)

(defpackage #:math/list-matr
  (:use #:cl)
  (:export rows
           cols
           dimensions
           col
           row
           average-value
           average-not-nil-value
           average-row-value
           average-row-not-nil-value
           average-col-value
           average-col-not-nil-value
           max-row-not-nil-value
           max-col-not-nil-value
           transpose
           detach-last-col
           get-last-col
           prepend-row
           prepend-col
           append-row
           append-col
           lv-print
           lm-print
           unite-rows
           make))

(in-package :math/list-matr)

(export 'transpose)

(defun transpose (2d-list)
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
"
  (apply #'mapcar #'list 2d-list))

(export 'unite-rows)

(defun unite-rows (lst)
  "@b(Описание:) функция @b(unite-rows) объединяет строки матрицы
(список списков) в вектор (список).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (unite-rows '((1 2 3)
               (4 5 6)))
 =>(1 2 3 4 5 6)
@end(code)
"
  (let ((rez nil))
    (mapcar #'(lambda (el) (setf rez (append rez el) ) ) lst)
    rez))

(export 'make)

(defun make (rows cols lst)
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
"
  (let ((rez nil)
        (rw nil))
    (dotimes (r rows (reverse rez))
      (dotimes (c cols)
        (push (nth (+ (* r cols) c) lst) rw))
      (push (reverse rw) rez)
      (setf rw nil))))

(export 'rows)

(defun rows (2d-list)
    "@b(Описание:) функция @b(rows) возвращает
количество строк в матрице, заданной списком 2d-list

 @b(Пример использования:)
@begin[lang=lisp](code)
 (rows '((0 1 2) (3 4 5)))  =>2
@end(code)
"
  (length 2d-list))

(export 'cols)

(defun cols (2d-list)
  "@b(Описание:) функция @b(cols) возвращает 
количество столбцов в матрице, представленной списком 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (cols '((0 1 2)
         (3 4 5)))  =>3
 (cols '((0 1 2 10) 
         (3 4 5)))  =>3
@end(code)
"
  (math/stat:min-value (mapcar #'length 2d-list)))

(export 'dimensions)

(defun dimensions (2d-list)
  "@b(Описание:) функция @b(cols) возвращает 
количество столбцов в матрице, представленной списком 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dimensions '((0 1 2)
               (3 4 5)))    => (2 3)
 (dimensions '((0 1 2 10) 
               (3 4 5   ))) => (2 3)
@end(code)
"
  (list (rows 2d-list) (cols 2d-list)))

(export 'row)

(defun row (row 2d-list)
  "@b(Описание:) функция @b(row) возвращает строку row матрицы,
представленной списком 2d-list.

 @b(Примечание:) Нумерация строк начинается с 0.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (row 1 '((0 1 2) 
          (3 4 5)))
 =>(3 4 5)
@end(code)
"
  (nth row 2d-list))

(export 'col)

(defun col (col 2d-list)
  "@b(Описание:) функция @b(col) возвращает столбец
@b(col) матрицы, представленной списком @b(2d-list).

 @b(Примечание:) Нумерация столбцов начинается с 0

 @b(Пример использования:)
@begin[lang=lisp](code)
 (col 1 '((0 1 2) 
          (3 4 5)))
 =>(1 4)
@end(code)"
  (mapcar #'(lambda (el) (nth col el)) 2d-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; average - Вычисление среднего значения
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'average-value)

(defun average-value (lst)
  "@b(Описание:) функция @b(average-value) вычисляет среднее значение по
элементам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-value '((1.0 1.5 2.0)
                  (2.0 2.5 3.0))) => 2.0
@end(code)"
  (math/stat:average-value (unite-rows lst)))

(export 'average-not-nil-value)

(defun average-not-nil-value (lst)
  "@b(Описание:) функция @b(average-not-nil-value) вычисляет среднее значение
по элементам матрицы (списка списков) с исключением nil-элементов."
  (math/stat:average-not-nil-value (unite-rows lst)))

;;;;

(export 'average-row-value)

(defun average-row-value (lst)
  "@b(Описание:) функция @b(average-row-value) вычисляет 
средние значения в строках матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-row-value '((1.0 1.5 2.0)
                      (2.0 2.5 3.0))) => (1.5 2.5)
@end(code)
"
  (mapcar #'math/stat:average-value lst))

(export 'average-row-not-nil-value)

(defun average-row-not-nil-value (lst)
  "@b(Описание:) функция @b(average-row-not-nil-value) вычисляет среднее значение
по элементам матрицы (списка списков)."
  (mapcar #'math/stat:average-not-nil-value lst))

(export 'max-row-not-nil-value)

(defun max-row-not-nil-value (lst)
  "@b(Описание:) функция @b(max-row-not-nil-value) вычисляет максимальные 
значения по строкам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (max-row-not-nil-value '((1.0 2.1 1.5 2.0)
                          (2.0 2.5 3.2 3.0))) => (2.1 3.2)
@end(code)
"
  (mapcar #'(lambda (el) (apply #'max (math/core:exclude-nil-from-list el))) lst))

;;;;

(export 'average-col-value)

(defun average-col-value (lst)
  "@b(Описание:) функция @b(average-col-value) вычисляет среднее значение
по столбцам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-col-value '((3.0 2.1 4.5)
                     (2.0 2.5 3.2))) => (2.5 2.3 3.85)
@end(code)"
  (average-row-value (transpose lst)))

(export 'average-col-not-nil-value)

(defun average-col-not-nil-value (lst)
  "@b(Описание:) функция @b(average-col-not-nil-value) вычисляет среднее 
значение по элементам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-col-not-nil-value '((nil 2.1 4.5)
                             (2.0 nil 3.2))) => (2.0 2.1 3.85)
  
@end(code)
"
  (average-row-not-nil-value (transpose lst)))

(export 'max-col-not-nil-value)

(defun max-col-not-nil-value (lst)
  "@b(Описание:) функция @b(max-col-not-nil-value) вычисляет среднее 
значение по столбцам матрицы (списка списков)."
  (max-row-not-nil-value (transpose lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; lm-print


(export 'lm-print)

(defun lm-print (2d-list &key (fmt "~6,1f") (stream t))
  "@b(Описание:) функция @b(lm-print) красивый вывод
матрицы (списка списков)."
  (format stream (concatenate 'string  "~{~{" fmt "~^ ~}~%~}") 2d-list))

(export 'lv-print)

(defun lv-print (lst &key (fmt "~6,1f") (stream t))
  "@b(Описание:) функция @b(lv-print) красивый вывод
 вектора (списка)."
  (format stream (concatenate 'string  "~{" fmt "~^ ~}~%") lst) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; append


(export 'append-col)

(defun append-col (c-lst 2d-list)
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
"
  (let ((rez nil)
        (r nil))
    (dolist (l 2d-list (reverse rez))
      (setf r (car c-lst)
            c-lst (cdr c-lst))
      (push (append l (list r)) rez))))

(export 'append-row)

(defun append-row (c-lst 2d-list)
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
"
  (transpose (append-col c-lst (transpose 2d-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; prepend


(export 'prepend-col)

(defun prepend-col (c-lst 2d-list)
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
"
  (let ((rez nil)
        (r nil))
    (dolist (l 2d-list (reverse rez))
      (setf r (car c-lst)
            c-lst (cdr c-lst))
      (push (cons r l) rez))))

(export 'prepend-row)

(defun prepend-row (c-lst 2d-list)
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
"
  (transpose (prepend-col c-lst (transpose 2d-list))))

(export 'detach-last-col)

(defun detach-last-col (2d-list)
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
"
  (mapcar
   #'(lambda(el) (reverse (cdr (reverse el))))
   2d-list))

(export 'get-last-col)

(defun get-last-col (2d-list)
  "@b(Описание:) функция @b(get-last-col) возвращает последний столбец матрицы, 
представленной в виде списка.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (get-last-col
 '((1  2  3  6)
   (2  3  4  9) 
   (3  4  5 12))) =>(6 9 12) 
@end(code)
"
  (mapcar
   #'(lambda(el) (car (last el)))
   2d-list))
