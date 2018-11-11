;;;; list-matr.lisp

;;; В данном файле определяются некоторые операции над матрицами,
;;; представленными 2d-list, (списком состоящим из списков)

(in-package #:math)

(defun list-matr-transpose (2d-list)
  "Выполняет транспонирование матрицы, представленной списком 2d-list
Пример использования:
 (list-matr-transpose '((1 2 3)(4 5 6)))
 =>((1 4) (2 5) (3 6))"
  (apply #'mapcar #'list 2d-list))

(defun list-matr-union (lst)
  "Объединяет строки матрицы (список списков) в вектор (список)
Пример использования:
 (list-matr-union '((1 2 3)(4 5 6)))
 =>(1 2 3 4 5 6) "
  (let ((rez nil))
    (mapcar #'(lambda (el) (setf rez (append rez el) ) ) lst)
    rez))

(defun list-matr-make (rows cols lst)
  "Генерирует матрицу (список списков) из вектора (списока)
Примеры использования:
 (math:list-matr-make 2 3 '(0 1 2 3 4 5 6 7 8 9 10 11 12 ))
 => ((0 1 2) (3 4 5))
 (math:list-matr-make 2 3 'nil)
 => ((NIL NIL NIL) (NIL NIL NIL))
"
  (let ((rez nil)
	(rw nil))
    (dotimes (r rows (reverse rez))
      (dotimes (c cols)
	(push (nth (+ (* r cols) c) lst) rw))
      (push (reverse rw) rez)
      (setf rw nil))))

(defun list-matr-rows (2d-list)
  "Возвращает количество строк в матрице, заданной списком 2d-list
Пример использования:
 (list-matr-rows '((0 1 2) (3 4 5)))
 =>2"
  (length 2d-list))

(defun list-matr-cols (2d-list)
  "Возвращает количество столбцов в матрице, представленной списком 2d-list.
Примеры использования:
 (list-matr-cols '((0 1 2) (3 4 5)))
 =>3
 (list-matr-cols '((0 1 2 10) (3 4 5)))
 =>3"
  (min-value (mapcar #'length 2d-list)))

(defun list-matr-row (row 2d-list)
  "Возвращает строку row матрицы, представленной списком 2d-list.
Примечание: Нумерация строк начинается с 0.
Пример использования:
 (list-matr-row 1 '((0 1 2) (3 4 5)))
 =>(3 4 5)"
  (nth row 2d-list))

(defun list-matr-col (col 2d-list)
  "Возвращает столбец col матрицы, представленной списком 2d-list.
Примечание: Нумерация столбцов начинается с 0
 (list-matr-col 1 '((0 1 2) (3 4 5)))
 =>(1 4)"
  (mapcar #'(lambda (el) (nth col el)) 2d-list))

;;;; averange - Вычисление среднего значения

(defun list-matr-averange-value (lst)
  "Вычисляет среднее значение по элементам матрицы (списка списков)"
  (averange-value (list-matr-union lst)))

(defun list-matr-averange-not-nil-value (lst)
    "Вычисляет среднее значение по элементам матрицы (списка списков) с исключением nil-элементов"
    (averange-not-nil-value (list-matr-union lst)))

;;;;

(defun list-matr-averange-row-value (lst)
  "Вычисляет средние значения в строках матрицы (списка списков)"
  (mapcar #'averange-value lst))

(defun list-matr-averange-row-not-nil-value (lst)
  "Вычисляет среднее значение по элементам матрицы (списка списков)"
  (mapcar #'averange-not-nil-value lst))

;;;;

(defun list-matr-averange-col-value (lst)
  "Вычисляет среднее значение по элементам матрицы (списка списков)"
  (list-matr-averange-row-value (list-matr-transpose lst)))

(defun list-matr-averange-col-not-nil-value (lst)
  "Вычисляет среднее значение по элементам матрицы (списка списков)"
  (list-matr-averange-row-not-nil-value (list-matr-transpose lst)))

;;;; print

(defun list-matr-print (lst &key (fmt "~6,1f"))
  "Красивый вывод матрицы (списка списков)"
  (mapc #'(lambda (el) (format t (concatenate 'string  "~{" fmt "~^ ~}~%") el) ) lst)
  nil)

(defun list-vector-print (lst &key (fmt "~6,1f"))
  "Красивый вывод вектора (списка)"
  (format t (concatenate 'string  "~{" fmt "~^ ~}~%") lst) )

;;;; append 

(defun list-matr-append-col (c-lst 2d-list)
  "Добавляет к матрице, представленной списком 2d-list, столбец (список) c-lst.
Пример использования:
 (math:list-matr-append-col '(10 11 12) '((1 2 3)(4 5 6)(7 8 9)))
 =>((1 2 3 10) (4 5 6 11) (7 8 9 12))
"
  (let ((rez nil)
	(r nil))
    (dolist (l 2d-list (reverse rez))
      (setf r (car c-lst)
	    c-lst (cdr c-lst))
      (push (append l (list r)) rez))))

(defun list-matr-append-row (c-lst 2d-list)
  "Добавляет вектор (список) r-lst к матрице 2d-list.
Пример использования:
 (math:list-matr-append-row '(10 11 12) '((1 2 3)(4 5 6)(7 8 9)))
 =>((1 2 3) (4 5 6) (7 8 9) (10 11 12))
 (math:list-matr-append-row '(10 11 ) '((1 2 3)(4 5 6)(7 8 9)))
 =>((1 2 3) (4 5 6) (7 8 9) (10 11 NIL))
 (math:list-matr-append-row '(10 11 12 13) '((1 2 3)(4 5 6)(7 8 9)))
 =>((1 2 3) (4 5 6) (7 8 9) (10 11 12))
"
  (list-matr-transpose (list-matr-append-col c-lst (list-matr-transpose 2d-list))))

;;;; prepend

(defun list-matr-prepend-col (c-lst 2d-list)
  "Добавляет вектор (список) c-lst к матрице 2d-list.
Пример использования:
 (math:list-matr-prepend-col '(10 11 12) '((1 2 3)(4 5 6)(7 8 9)))
 =>((10 1 2 3) (11 4 5 6) (12 7 8 9))
"
  (let ((rez nil)
	(r nil))
    (dolist (l 2d-list (reverse rez))
      (setf r (car c-lst)
	    c-lst (cdr c-lst))
      (push (cons r l) rez))))

(defun list-matr-prepend-row (c-lst 2d-list)
  "Пример использования:
 (math:list-matr-prepend-row '(10 11 12) '((1 2 3)(4 5 6)(7 8 9)))
 =>((10 11 12) (1 2 3) (4 5 6) (7 8 9))
 (math:list-matr-prepend-row '(10 11 ) '((1 2 3)(4 5 6)(7 8 9)))
 =>((10 11 NIL) (1 2 3) (4 5 6) (7 8 9))
 (math:list-matr-prepend-row '(10 11 12 13) '((1 2 3)(4 5 6)(7 8 9)))
 =>((10 11 12) (1 2 3) (4 5 6) (7 8 9))
"
  (list-matr-transpose (list-matr-prepend-col c-lst (list-matr-transpose 2d-list))))
