;;;; list-matr.lisp

;;; В данном файле определяются некоторые операции над матрицами,
;;; представленными 2d-list, (списком состоящим из списков)

;;;; TODO list-matr-___ -> math/list-matr:___

(defpackage #:math/list-matr
  (:use #:cl)
  (:export list-matr-average-not-nil-value
	   list-matr-detach-last-col
	   list-matr-prepend-col
	   list-matr-max-row-not-nil-value
	   list-matr-transpose list-vector-print
	   list-matr-average-col-not-nil-value
	   list-matr-average-row-not-nil-value
	   list-matr-append-row list-matr-append-col
	   list-matr-cols list-matr-print
	   list-matr-col list-matr-get-last-col
	   list-matr-union list-matr-average-col-value
	   list-matr-row list-matr-prepend-row
	   list-matr-make list-matr-rows
	   list-matr-average-value
	   list-matr-max-col-not-nil-value
	   list-matr-average-row-value))

(in-package :math/list-matr)

(export 'list-matr-transpose)

(defun list-matr-transpose (2d-list)
  "@b(Описание:) функция @b(list-matr-transpose) выполняет транспонирование матрицы,
представленной списком 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (list-matr-transpose '((1 2 3)
			(4 5 6)))
 =>((1 4) (2 5) (3 6))
@end(code)
"
  (apply #'mapcar #'list 2d-list))

(export 'list-matr-union)

(defun list-matr-union (lst)
  "@b(Описание:) функция @b(list-matr-union) объединяет строки матрицы
(список списков) в вектор (список).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (list-matr-union '((1 2 3)
                    (4 5 6)))
 =>(1 2 3 4 5 6)
@end(code)
"
  (let ((rez nil))
    (mapcar #'(lambda (el) (setf rez (append rez el) ) ) lst)
    rez))

(export 'list-matr-make)

(defun list-matr-make (rows cols lst)
  "@b(Описание:) функция @b(list-matr-make) генерирует матрицу 
(список списков) из вектора (списока).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (math:list-matr-make 2 3 '(0 1 2 3 4 5 6 7 8 9 10 11 12))
 => ((0 1 2) 
     (3 4 5))
 (math:list-matr-make 2 3 'nil)
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

(export 'list-matr-rows)

(defun list-matr-rows (2d-list)
    "Возвращает количество строк в матрице, заданной списком 2d-list

 @b(Пример использования:)
@begin[lang=lisp](code)
 (list-matr-rows '((0 1 2) (3 4 5)))  =>2
@end(code)
"
  (length 2d-list))

(export 'list-matr-cols)

(defun list-matr-cols (2d-list)
  "@b(Описание:) функция @b(list-matr-cols) возвращает 
количество столбцов в матрице, представленной списком 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (list-matr-cols '((0 1 2)
                   (3 4 5)))  =>3
 (list-matr-cols '((0 1 2 10) 
                   (3 4 5)))  =>3
@end(code)
"
  (min-value (mapcar #'length 2d-list)))

(export 'list-matr-row)

(defun list-matr-row (row 2d-list)
  "@b(Описание:) функция @b(list-matr-row) возвращает строку row матрицы,
представленной списком 2d-list.

 @b(Примечание:) Нумерация строк начинается с 0.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (list-matr-row 1 '((0 1 2) (3 4 5)))
 =>(3 4 5)
@end(code)
"
  (nth row 2d-list))

(export 'list-matr-col)

(defun list-matr-col (col 2d-list)
  "Возвращает столбец col матрицы, представленной списком 2d-list.

 @b(Примечание:) Нумерация столбцов начинается с 0
 (list-matr-col 1 '((0 1 2) (3 4 5)))
 =>(1 4)"
  (mapcar #'(lambda (el) (nth col el)) 2d-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; average - Вычисление среднего значения
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'list-matr-average-value)

(defun list-matr-average-value (lst)
  "@b(Описание:) функция @b(average-value) вычисляет среднее значение по
элементам матрицы (списка списков)."
  (average-value (list-matr-union lst)))

(export 'list-matr-average-not-nil-value)

(defun list-matr-average-not-nil-value (lst)
  "@b(Описание:) функция @b(average-not-nil-value) вычисляет среднее значение
по элементам матрицы (списка списков) с исключением nil-элементов."
  (average-not-nil-value (list-matr-union lst)))

;;;;

(export 'list-matr-average-row-value)

(defun list-matr-average-row-value (lst)
  "Вычисляет средние значения в строках матрицы (списка списков)"
  (mapcar #'average-value lst))

(export 'list-matr-average-row-not-nil-value)

(defun list-matr-average-row-not-nil-value (lst)
  "@b(Описание:) функция @b(list-matr-average-row-not-nil-value) вычисляет среднее значение
по элементам матрицы (списка списков)."
  (mapcar #'average-not-nil-value lst))

(export 'list-matr-max-row-not-nil-value)

(defun list-matr-max-row-not-nil-value (lst)
  "@b(Описание:) функция @b(list-matr-max-row-not-nil-value) вычисляет максимальные 
значения по строкам матрицы (списка списков)."
  (mapcar #'(lambda (el) (apply #'max (exclude-nil-from-list el))) lst))

;;;;

(export 'list-matr-average-col-value)

(defun list-matr-average-col-value (lst)
  "@b(Описание:) функция @b(list-matr-average-col-value) вычисляет среднее значение
по столбцам матрицы (списка списков)"
  (list-matr-average-row-value (list-matr-transpose lst)))

(export 'list-matr-average-col-not-nil-value)

(defun list-matr-average-col-not-nil-value (lst)
  "@b(Описание:) функция @b(list-matr-average-col-not-nil-value) вычисляет среднее 
значение по элементам матрицы (списка списков)."
  (list-matr-average-row-not-nil-value (list-matr-transpose lst)))

(export 'list-matr-max-col-not-nil-value)

(defun list-matr-max-col-not-nil-value (lst)
  "@b(Описание:) функция @b(list-matr-max-col-not-nil-value) вычисляет среднее 
значение по столбцам матрицы (списка списков)."
  (list-matr-max-row-not-nil-value (list-matr-transpose lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; print
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'list-matr-print)

(defun list-matr-print (lst &key (fmt "~6,1f"))
  "@b(Описание:) функция @b(list-matr-print) красивый вывод
матрицы (списка списков)."
  (mapc #'(lambda (el) (format t (concatenate 'string  "~{" fmt "~^ ~}~%") el) ) lst)
  
  nil)

(export 'list-vector-print)

(defun list-vector-print (lst &key (fmt "~6,1f"))
  "@b(Описание:) функция @b(list-vector-print) красивый вывод
 вектора (списка)."
  (format t (concatenate 'string  "~{" fmt "~^ ~}~%") lst) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; append
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'list-matr-append-col)

(defun list-matr-append-col (c-lst 2d-list)
  "@b(Описание:) функция @b(list-matr-append-col) добавляет к матрице,
представленной списком 2d-list, столбец (список) c-lst.

 @b(Пример использования:)
@begin[lang=lisp](code)
(math:list-matr-append-col '(10 11 12)
			   '((1 2 3)
			     (4 5 6)
			     (7 8 9)))
 => ((1 2 3 10) 
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

(export 'list-matr-append-row)

(defun list-matr-append-row (c-lst 2d-list)
  "@b(Описание:) функция @b(list-matr-append-row) добавляет вектор
(список) r-lst к матрице 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (math:list-matr-append-row '(10 11 12)
			    '((1 2 3)
			      (4 5 6)
			      (7 8 9)))
 =>((1 2 3)
    (4 5 6)
    (7 8 9)
    (10 11 12))

 (math:list-matr-append-row '(10 11 )
			    '((1 2 3)
			      (4 5 6)
			      (7 8 9)))
 =>((1 2 3)
    (4 5 6)
    (7 8 9)
    (10 11 NIL))

 (math:list-matr-append-row '(10 11 12 13)
			    '((1 2 3)
			      (4 5 6)
			      (7 8 9)))
 =>((1 2 3)
    (4 5 6)
    (7 8 9)
    (10 11 12))
@end(code)
"
  (list-matr-transpose (list-matr-append-col c-lst (list-matr-transpose 2d-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; prepend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'list-matr-prepend-col)

(defun list-matr-prepend-col (c-lst 2d-list)
  "@b(Описание:) функция @b(list-matr-prepend-col) добавляет вектор
(список) c-lst к матрице 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (math:list-matr-prepend-col '(10 11 12)
			     '((1 2 3)
			       (4 5 6)
			       (7 8 9)))
 =>((10 1 2 3)
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

(export 'list-matr-prepend-row)

(defun list-matr-prepend-row (c-lst 2d-list)
  "@b(Описание:) функция @b(list-matr-prepend-row)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (math:list-matr-prepend-row '(10 11 12) 
                             '((1 2 3)
                               (4 5 6)
                               (7 8 9)))
 =>((10 11 12) 
    ( 1  2  3) 
    ( 4  5  6)
    ( 7  8  9))
 (math:list-matr-prepend-row '(10 11 )
			     '((1 2 3)
			       (4 5 6)
			       (7 8 9)))
 =>((10 11 NIL) 
    (1 2 3) 
    (4 5 6) 
    (7 8 9))

 (math:list-matr-prepend-row '(10 11 12 13)
			     '((1 2 3)
			       (4 5 6)
			       (7 8 9)))
 =>((10 11 12) 
    ( 1  2  3) 
    ( 4  5  6)
    ( 7  8  9))
@begin[lang=lisp](code)
@end(code)
"
  (list-matr-transpose (list-matr-prepend-col c-lst (list-matr-transpose 2d-list))))

(export 'list-matr-detach-last-col)

(defun list-matr-detach-last-col (2d-list)
  "@b(Описание:) функция @b(list-matr-detach-last-col) возвращает матрицу, 
представленную в виде списка, образованную удалением последнего столбца 
(последнего элемента каждой строки).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (list-matr-detach-last-col
 '((1 2 3  6)
   (2 3 4  9) 
   (3 4 5 12)))
 =>((1 2 3) (2 3 4) (3 4 5))
@end(code)
"
  (mapcar
   #'(lambda(el) (reverse (cdr (reverse el))))
   2d-list))

(export 'list-matr-get-last-col)

(defun list-matr-get-last-col (2d-list)
  "@b(Описание:) функция @b(list-matr-get-last-col) возвращает последний столбец матрицы, 
представленной в виде списка.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (list-matr-get-last-col
 '((1 2 3  6)
   (2 3 4  9) 
   (3 4 5 12)))
 =>(6 9 12) 
@end(code)
"
  (mapcar
   #'(lambda(el) (car (last el)))
   2d-list))
