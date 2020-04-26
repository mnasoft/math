;;;; list-matr.lisp

;;; В данном файле определяются некоторые операции над матрицами,
;;; представленными 2d-list, (списком состоящим из списков)

(in-package #:math)
(annot:enable-annot-syntax)

@export
@annot.doc:doc
  "Выполняет транспонирование матрицы, представленной списком 2d-list
Пример использования:
 (list-matr-transpose '((1 2 3)(4 5 6)))
 =>((1 4) (2 5) (3 6))"
(defun list-matr-transpose (2d-list)
  (apply #'mapcar #'list 2d-list))

@export
@annot.doc:doc
  "Объединяет строки матрицы (список списков) в вектор (список)
Пример использования:
 (list-matr-union '((1 2 3)(4 5 6)))
 =>(1 2 3 4 5 6) "
(defun list-matr-union (lst)
  (let ((rez nil))
    (mapcar #'(lambda (el) (setf rez (append rez el) ) ) lst)
    rez))

@export
@annot.doc:doc
  "Генерирует матрицу (список списков) из вектора (списока)
Примеры использования:
 (math:list-matr-make 2 3 '(0 1 2 3 4 5 6 7 8 9 10 11 12 ))
 => ((0 1 2) (3 4 5))
 (math:list-matr-make 2 3 'nil)
 => ((NIL NIL NIL) (NIL NIL NIL))
"
(defun list-matr-make (rows cols lst)
  (let ((rez nil)
	(rw nil))
    (dotimes (r rows (reverse rez))
      (dotimes (c cols)
	(push (nth (+ (* r cols) c) lst) rw))
      (push (reverse rw) rez)
      (setf rw nil))))

@export
@annot.doc:doc
  "Возвращает количество строк в матрице, заданной списком 2d-list
Пример использования:
 (list-matr-rows '((0 1 2) (3 4 5)))
 =>2"
(defun list-matr-rows (2d-list)
  (length 2d-list))

@export
@annot.doc:doc
  "Возвращает количество столбцов в матрице, представленной списком 2d-list.
Примеры использования:
 (list-matr-cols '((0 1 2) (3 4 5)))
 =>3
 (list-matr-cols '((0 1 2 10) (3 4 5)))
 =>3"
(defun list-matr-cols (2d-list)
  (min-value (mapcar #'length 2d-list)))

@export
@annot.doc:doc
  "Возвращает строку row матрицы, представленной списком 2d-list.
Примечание: Нумерация строк начинается с 0.
Пример использования:
 (list-matr-row 1 '((0 1 2) (3 4 5)))
 =>(3 4 5)"
(defun list-matr-row (row 2d-list)
  (nth row 2d-list))

@export
@annot.doc:doc
"Возвращает столбец col матрицы, представленной списком 2d-list.
Примечание: Нумерация столбцов начинается с 0
 (list-matr-col 1 '((0 1 2) (3 4 5)))
 =>(1 4)"
(defun list-matr-col (col 2d-list)
  (mapcar #'(lambda (el) (nth col el)) 2d-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; average - Вычисление среднего значения
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"Вычисляет среднее значение по элементам матрицы (списка списков)"
(defun list-matr-average-value (lst)
  (average-value (list-matr-union lst)))

@export
@annot.doc:doc
"Вычисляет среднее значение по элементам матрицы (списка списков) с исключением nil-элементов"
(defun list-matr-average-not-nil-value (lst)
    (average-not-nil-value (list-matr-union lst)))

;;;;

@export
@annot.doc:doc
"Вычисляет средние значения в строках матрицы (списка списков)"
(defun list-matr-average-row-value (lst)
  (mapcar #'average-value lst))

@export
@annot.doc:doc
"Вычисляет среднее значение по элементам матрицы (списка списков)"
(defun list-matr-average-row-not-nil-value (lst)
  (mapcar #'average-not-nil-value lst))

@export
@annot.doc:doc
"Вычисляет максимальные значения по строкам матрицы (списка списков)"
(defun list-matr-max-row-not-nil-value (lst)
   (mapcar #'(lambda (el) (apply #'max (exclude-nil-from-list el))) lst))

;;;;

@export
@annot.doc:doc
"Вычисляет среднее значение по столбцам матрицы (списка списков)"
(defun list-matr-average-col-value (lst)
  (list-matr-average-row-value (list-matr-transpose lst)))

@export
@annot.doc:doc
"Вычисляет среднее значение по элементам матрицы (списка списков)"
(defun list-matr-average-col-not-nil-value (lst)
  (list-matr-average-row-not-nil-value (list-matr-transpose lst)))

@export
@annot.doc:doc
"Вычисляет среднее значение по столбцам матрицы (списка списков)"
(defun list-matr-max-col-not-nil-value (lst)
  (list-matr-max-row-not-nil-value (list-matr-transpose lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; print
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"Красивый вывод матрицы (списка списков)"
(defun list-matr-print (lst &key (fmt "~6,1f"))
  (mapc #'(lambda (el) (format t (concatenate 'string  "~{" fmt "~^ ~}~%") el) ) lst)
  nil)

@export
@annot.doc:doc
"Красивый вывод вектора (списка)"
(defun list-vector-print (lst &key (fmt "~6,1f"))
  (format t (concatenate 'string  "~{" fmt "~^ ~}~%") lst) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; append
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"Добавляет к матрице, представленной списком 2d-list, столбец (список) c-lst.
Пример использования:
 (math:list-matr-append-col '(10 11 12) '((1 2 3)(4 5 6)(7 8 9)))
 =>((1 2 3 10) (4 5 6 11) (7 8 9 12))
"
(defun list-matr-append-col (c-lst 2d-list)
  (let ((rez nil)
	(r nil))
    (dolist (l 2d-list (reverse rez))
      (setf r (car c-lst)
	    c-lst (cdr c-lst))
      (push (append l (list r)) rez))))

@export
@annot.doc:doc
  "Добавляет вектор (список) r-lst к матрице 2d-list.
Пример использования:
 (math:list-matr-append-row '(10 11 12) '((1 2 3)(4 5 6)(7 8 9)))
 =>((1 2 3) (4 5 6) (7 8 9) (10 11 12))
 (math:list-matr-append-row '(10 11 ) '((1 2 3)(4 5 6)(7 8 9)))
 =>((1 2 3) (4 5 6) (7 8 9) (10 11 NIL))
 (math:list-matr-append-row '(10 11 12 13) '((1 2 3)(4 5 6)(7 8 9)))
 =>((1 2 3) (4 5 6) (7 8 9) (10 11 12))
"
(defun list-matr-append-row (c-lst 2d-list)
  (list-matr-transpose (list-matr-append-col c-lst (list-matr-transpose 2d-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; prepend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
  "Добавляет вектор (список) c-lst к матрице 2d-list.
Пример использования:
 (math:list-matr-prepend-col '(10 11 12) '((1 2 3)(4 5 6)(7 8 9)))
 =>((10 1 2 3) (11 4 5 6) (12 7 8 9))
"
(defun list-matr-prepend-col (c-lst 2d-list)
  (let ((rez nil)
	(r nil))
    (dolist (l 2d-list (reverse rez))
      (setf r (car c-lst)
	    c-lst (cdr c-lst))
      (push (cons r l) rez))))

@export
@annot.doc:doc
"Пример использования:
 (math:list-matr-prepend-row '(10 11 12) '((1 2 3)(4 5 6)(7 8 9)))
 =>((10 11 12) (1 2 3) (4 5 6) (7 8 9))
 (math:list-matr-prepend-row '(10 11 ) '((1 2 3)(4 5 6)(7 8 9)))
 =>((10 11 NIL) (1 2 3) (4 5 6) (7 8 9))
 (math:list-matr-prepend-row '(10 11 12 13) '((1 2 3)(4 5 6)(7 8 9)))
 =>((10 11 12) (1 2 3) (4 5 6) (7 8 9))
"
(defun list-matr-prepend-row (c-lst 2d-list)
  (list-matr-transpose (list-matr-prepend-col c-lst (list-matr-transpose 2d-list))))

@export
@annot.doc:doc
  "Возвращает матрицу, представленную в виде списка,
образованную удалением последнего столбца (последнего 
элемента каждой строки).
Пример использования:
 (list-matr-detach-last-col
 '((1 2 3  6)
   (2 3 4  9) 
   (3 4 5 12)))
 =>((1 2 3) (2 3 4) (3 4 5)) "
(defun list-matr-detach-last-col (2d-list)
  (mapcar
   #'(lambda(el) (reverse (cdr (reverse el))))
   2d-list))

@export
@annot.doc:doc
  "Возвращает последний столбец матрицы, представленной в виде списка.
Пример использования:
 (list-matr-get-last-col
 '((1 2 3 6)
  (2 3 4 9) 
  (3 4 5 12)))
 =>(6 9 12) "
(defun list-matr-get-last-col (2d-list)
  (mapcar
   #'(lambda(el) (car (last el)))
   2d-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


