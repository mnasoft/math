;;;; list-matr.lisp

(in-package #:math)

(defun list-matr-transpose (list)
  "Выполняет транспонирование"
  (apply #'mapcar #'list list))

(defun list-matr-union (lst)
  "Объединяет строки матрицы (список списков) в вектор (список)"
  (let ((rez nil))
    (mapcar #'(lambda (el) (setf rez (append rez el) ) ) lst)
    rez))

(defun list-matr-make (rows cols lst)
  "Генерирует матрицу (список списков) из вектора (списока)"
  (let ((rez nil)
	(rw nil))
    (dotimes (r rows (reverse rez))
      (dotimes (c cols)
	(push (nth (+ (* r cols) c) lst) rw))
      (push (reverse rw) rez)
      (setf rw nil))))

(defun list-matr-rows (lst)
  (length lst))

(defun list-matr-cols (lst)
  (min-value (mapcar #'length lst)))

(defun list-matr-row (row lst)
  "Нумерация строк начинается с 0"
  (nth row lst))

(defun list-matr-col (col lst)
  "Нумерация столбцов начинается с 0"
  (mapcar #'(lambda (el) (nth col el)) lst))

;;;; averange

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

(defun list-matr-append-col (c-lst lst)
  "Добавляет вектор (список) c-lst к матрице lst"
  (let ((rez nil)
	(r nil))
    (dolist (l lst (reverse rez))
      (setf r (car c-lst)
	    c-lst (cdr c-lst))
      (push (append l (list r)) rez))))

(defun list-matr-append-row (c-lst lst)
  "Добавляет вектор (список) r-lst к матрице lst"
  (list-matr-transpose (list-matr-append-col c-lst (list-matr-transpose lst))))

;;;; prepend

(defun list-matr-prepend-col (c-lst lst)
  (let ((rez nil)
	(r nil))
    (dolist (l lst (reverse rez))
      (setf r (car c-lst)
	    c-lst (cdr c-lst))
      (push (cons r l) rez))))

(defun list-matr-prepend-row (c-lst lst)
  (list-matr-transpose (list-matr-prepend-col c-lst (list-matr-transpose lst))))
