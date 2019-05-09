;;;; matr-class-temp.lisp

(in-package #:math)

;;;;

(defun matr-mult (a b)
  "Выполняет перемножение матриц a и b
(matr-mult  '(\"Matr\" 2 3
	     ((0 . 1.0) (1 . 2.0) (2 . 3.0)
	      (3 . 4.0) (4 . 5.0) (5 . 6.0)))
	     '(\"Matr\" 3 2
	       ((0 . 1.0) (1 . 2.0)
		(2 . 3.0) (3 . 4.0)
		(4 . 5.0) (5 . 6.0)))
	     )
"
  (let ((a_n (matr-rows a))
	(a_m (matr-cols a))
	(b_n (matr-rows b))
	(b_m (matr-cols b))
	(c nil))
    (cond ((/= a_m b_n)
	   (break
	    "Запрещенные размеры матриц для перемножения: A[~A,~A] x B[~A,~A]"
	    a_n a_m b_n b_m))
	  (t (setf c (matr-new a_n b_m))))
    (do ((i 0 (1+ i))
	 (a-row nil))
	((>= i a_n))
      (setf a-row (matr-get-row a i))
      (do ((j 0 (1+ j))
	   (b-col nil))
	  ((>= j b_m))
	(setf b-col (matr-get-col b j))
	(setf c (matr-set_ij c (apply #'+ (mapcar #'* a-row b-col)) i j))
	))
    c))

(defun matr->2d-list (matr)
  "Выполняет преобразование матрицы в 2d-list (список списков).
Пример использования:
 (matr->2d-list (matr-new 3 2 '( 1.0 2.0 3.0 11.0 12.0 13.0)))
 =>(1.0 2.0) (3.0 11.0) (12.0 13.0))
"
  (let ((r (matr-rows  matr )))
    (loop :for i :from 0 :below r
       collect   (matr-get-row  matr i))))

(defun 2d-list->matr (2d-list)
  "Выполняет преобразование 2d-list (списка списков) в матрицу
Пример использования:
 (2d-list->matr '((1.0 2.0) (3.0 11.0) (12.0 13.0)))
 =>(\"Matr\" 2 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0) (3 . 11.0) (4 . 12.0) (5 . 13.0)))
"
  (let ((r    (list-matr-cols  2d-list))
	(c    (list-matr-rows  2d-list))
	(data (list-matr-union 2d-list)))
    (matr-new r c data)))

(defun matr-to-point (matr)
  "Выполняет преобразование матрицы в точку
 (matr-to-point (matr-new 1 3 '(1.0 2.0 3.0)))
 => (1.0 2.0 3.0)
 (matr-to-point (matr-new 3 1 '(1.0 2.0 3.0)))
 => (1.0 2.0 3.0)
 (matr-to-point (matr-new 3 2 '( 1.0 2.0 3.0 11.0 12.0 13.0)))
 => (1.0 2.0 3.0 11.0 12.0 13.0)
 (matr-to-point (matr-new 2 3 '(1.0 2.0 3.0 11.0 12.0 13.0)))
 => (1.0 2.0 3.0 11.0 12.0 13.0)
"
  (mapcar #'cdr (matr-elements matr)))

(defun point-to-matr (p)
  "Выполняет преобразование точки в матрицу
;;;;
Пример использования:
(point-to-matr '(10.0 11.0 12.0 13.0))
=> (\"Matr\" 1 4 ((0 . 10.0) (1 . 11.0) (2 . 12.0) (3 . 13.0)))
"
  (matr-new 1 (length p) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



