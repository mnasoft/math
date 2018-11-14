;;;; matr-class.lisp

(in-package #:math)

(defclass matrix ()
  ((data :accessor matrix-data :initform nil :initarg :data)))

(defmethod print-object ((mm matrix) s)
  (format s "~A " (matr-name))
  (when (and (matrix-data mm) (arrayp (matrix-data mm)))
    (format s "~{~A~^х~}" (array-dimensions (matrix-data mm)))
    (loop :for i :from 0 :below (array-dimension (matrix-data mm) 0)
       :do
	 (format s "~%[")
	 (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1)
	    :do (format s " ~8A " (aref (matrix-data mm) i j)))
	 	 (format s "]"))))

(defun matr-new (rows cols &optional (lst nil))
  "Примечание:
 (matr-new 3 4 '(1 2 3 4 5 6 7 8 9 10)) "
  (let ((mm (make-instance 'matrix :data  (make-array (list rows cols) :initial-element 0.0d0)))
	(ll lst))
        (loop :for i :from 0 :below (array-dimension (matrix-data mm) 0)
       :do
	 (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1)
	    :do (setf (aref (matrix-data mm) i j) (car ll)
		      ll (cdr ll))))
	mm))

(defun matr-name ()
  "Возвращает сторку \"Matr\"
;;;;
Пример использования:
(matr-name)
"
  "Matr")

;;;;

(defun matr-idx (i j m)
  "Вычисляет индекс (i,j) элемента матрицы индексы начинаются с нуля
m-количест во столбцов в матрице"
  (+ (* i m) j))

(defun matr-index (matr i j)
  "Вычисляет индекс (i,j) элемента матрицы индексы начинаются с нуля
m-количест во столбцов в матрице"
  (matr-idx i j (matr-cols matr)))

(defun matr-elements (matr)
  "Возвращает список элементов матрицы
Пример использования:
(matr-elements '(\"Matr\" 2 3 ((0 . 0.0) (1 . 0.0) (2 . 0.0) (3 . 0.0) (4 . 0.0) (5 . 0.0))))
;
=>((0 . 0.0) (1 . 0.0) (2 . 0.0) (3 . 0.0) (4 . 0.0) (5 . 0.0))"
  (cadddr matr))

(defun matr-ind_err (i j n m)
  "Пример использования:
(matr-ind_err 0 0 2 3)
"
  (format nil "Ошибка при доступе к эл. матрицы: M[~A,~A] M[0..~A,0..~A]" i j n m))

(defun in-range-inc (i i_min i_max ) (<= i_min i  i_max))

(defun in-range-exc (i i_min i_max) (< i_min i  i_max))

(defun in-range-inc-exc (i i_min i_max) (and (<= i_min i) (< i i_max)))

(defun matr-index-is-good (matr i j)
  (let ((n  (matr-rows matr))
	(m  (matr-cols matr)))
    (if (not (and(in-range-inc-exc i 0 n) (in-range-inc-exc j 0 m)))
	(break "Ошибка при доступе к эл. матрицы: M[~A,~A] M[0..~A,0..~A]" i j (- n 1) (-  m 1 )))))

(defun matr-set_ij (matr elem i j)
  "Выполняет присваивание значения elem элементу матрицы matr(i j) элементы начинаются с нуля;
Пример использования:
(matr-set_ij 
 '(\"Matr\" 2 3 ((0 . 0.0) (1 . 0.0) (2 . 0.0) 
		 (3 . 0.0) (4 . 0.0) (5 . 0.0)))
 555.0 
 1 1)
"
  (let ((n  (cadr matr))
	(m  (caddr matr))
	(li (cadddr matr)))
    (matr-index-is-good matr i j)
    (setf li (subst (cons (matr-idx i j m) elem)
		    (assoc (matr-idx i j m) li)
		    li))
    (list (matr-name) n m li)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod matr-rows-* ((mm matrix)) (array-dimension (matrix-data mm) 0))

(defmethod matr-cols-* ((mm matrix)) (array-dimension (matrix-data mm) 1))

(defmethod matr-ij-*   ((mm matrix) i j) (aref (matrix-data mm) i j))

(defmethod matr-set-ij-* ((mm matrix) value i j) (setf (aref (matrix-data mm) i j) value))

(defmethod matr-set-row-* ((mm matrix) row pts)
  (let ((data (matrix-data mm))
	(ll pts))
    (loop :for c :from 0 :below (matr-cols-* mm)
       :do (setf (aref data row c) (car ll)
		 ll (cdr ll)))))

(defmethod matr-get-row-* ((mm matrix) row)
  (let ((data (matrix-data mm)))
    (loop :for c :from 0 :below (matr-cols-* mm)
       :collect (aref data row c))))

(matr-get-row-* *mm* 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun matr-set-col(matr j pts)
  "Пример использования;
(matr-set-col
 '(\"Matr\" 2 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0) 
		 (3 . 4.0) (4 . 5.0) (5 . 6.0)))
 0 '(11.0 12.0))"
  (do ((n (matr-rows matr))
       (i 0 (1+ i)))
      ((>= i n) matr)
    (setf matr (matr-set_ij matr (nth i pts)i j))))

(defun matr-get-col (matr j)
  "Пример использования:
(matr-get-col
 '(\"Matr\" 2 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0) 
		 (3 . 4.0) (4 . 5.0) (5 . 6.0)))
 1)
"
  (do ((n (matr-rows matr))
       (i 0 (1+ i))
       (pts nil))
      ((>= i n)(reverse pts))
    (setf pts (cons (matr-ij matr i j) pts))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun matr-eval (matr)
  "Выполняет поэлементное оценивание каждого элемента матрицы
Пример использования:
(progn
  (defparameter α (/ pi 3))
  (let ((matr '(\"Matr\" 2 3
		((0 . (sin α))  (1 . 0.0)     (2 . 0.0)
		 (3 . 0.0)      (4 . (cos α)) (5 . 0.0)))))
    (matr-eval matr)))
"
  (let ((n  (matr-rows matr))
	(m  (matr-cols matr))
	(li (matr-elements matr)))
    (setf li (mapcar #'(lambda (el) (cons (car el) (eval (cdr el)))) li))
    (list (matr-name) n m li)))

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

(defun matr-copy (matr)
  "Возвращает копию матрицы;
Пример использования: 
(matr-print 
 (matr-copy 
  '(\"Matr\" 3 4 ((0 . 1.0)  (1 . 2.0) (2 . 3.0) (3 . 14.0) 
		  (4 . 2.0)  (5 . 1.0) (6  . 1.0) (7  . 7.0) 
		  (8 . 3.0)  (9 . 0.0) (10 . 1.0) (11 . 2.0)))))
"
  (matr-new (matr-rows matr) (matr-cols matr) (matr-to-point matr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun matr-to-string (matr)
  "Выводит представление матрицы в виде строки символов
;;;;
Пример использования:
(matr-to-string '( \"Matr\" 2 3
	     ((0 . (sin *alfa*)) (1 . 0.0) (2 . 0.0)
	      (3 . 0.0) (4 . (cos *alfa*)) (5 . 0.0))))
"
  (let ((n (matr-rows matr))
	(m (matr-cols matr))
	(out (make-string-output-stream)))
    (format out "Matr ~S ~S~%" n m)
    (do ((i 0 (1+ i))
	 )
	((>= i n) (get-output-stream-string out))
      (format out "~S~%" (matr-get-row matr i)))))

(defun matr-print (matr &optional (ostream t))
  "Выводит представление матрицы в поток
;;;;
Пример использования:
(matr-print '(\"Matr\" 2 3
	     ((0 . (sin *alfa*)) (1 . 0.0) (2 . 0.0)
	      (3 . 0.0) (4 . (cos *alfa*)) (5 . 0.0))))"
  (format ostream "~A" (matr-to-string matr)))

(defun matr-mnk(vv ff ex_pts)
  "Формирует точки для расчета коэффициентов по методу наименьших квадратов
vv     - '(xx yy) - список, состоящий из имен факторов влияния
         и имени функции отклика;
ff     - '((xx xx) (xx) (1.0) (yy)) - задает 
         вид функциональной зависимости;
ex_pts - '((-1.0 1.0) (2.0 4.0) (3.0 9.0))  - задает 
         значения факторов влияния и значение функции отклика
;
Пример использования:
(matr-las-gauss  
 (matr-mnk '(xx yy) 
	   '((xx xx) (xx) (1.0) (yy)) 
	   '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0))))
;
=>(\"Matr\" 1 3 ((0 . 1.0d0) (1 . 0.0d0) (2 . 0.0d0)))
"
  (let* ((m          (length ff))
	 (n          (1- m))
	 (mtr        (matr-new n m))
	 (mtr-lambda (matr-new n m)))
    (dotimes (i n)
      (dotimes (j m)
	(matr-set-ij
	 mtr-lambda
	 (eval (list 'lambda  vv (cons '* (append (nth i ff) (nth j ff))))) i j)))
    (mapc
     #'(lambda (el)
	 (dotimes (i n)
	   (dotimes (j m)
	     (matr-set-ij
	      mtr
	      (+ (apply (matr-ij mtr-lambda i j) el)
		 (matr-ij mtr i j))
	      i j))))
     ex_pts)
    mtr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun matr-triang (matr)
  "Выполняет приведение  матрицы  к треугольному виду, для решения системы ЛУ методом Гаусса;
Пример использования 1
(matr-print 
 (matr-triang (matr-new 3 4 '(0.0 0.0 4.0 12.0
			      2.0 0.0 2.0 8.0
			      0.0 3.0 0.0 6.0))))
=> 
Matr 3 4
(1.0 0.0 1.0 4.0)
(0.0 1.0 0.0 2.0)
(0.0 0.0 1.0 3.0)
;
Пример использования 2
;
(matr-print 
 (matr-triang (matr-new 3 4 '(1.0d0  2.0d0  3.0d0  4.0d0 
			      5.0d0  6.0d0  7.0d0  8.0d0
			      9.0d0 10.0d0 11.0d0 12.0d0))))
=>
Matr 3 4
(1.0D0 2.0D0 3.0D0 4.0D0)
(-0.0D0 1.0D0 2.0D0 2.9999999999999996D0)
(0.0D0 0.0D0 0.0D0 8.881784197001252D-16)
"
  (do ((n (matr-rows matr)) (ie nil) (j 0 (1+ j)) (row_j nil) (row_i nil))
      ((>= j n) matr)
    (setf ie (1- n))
    (do ((i j (1+ i)) (matr_ij nil) (row_ie nil)) ; Цикл перестановки строк в j-товом столбце которых присутстыуют нули
	((> i ie))
      (setf row_i   (matr-get-row matr i)
	    matr_ij (matr-ij matr i j))
      (cond ((= matr_ij 0) ; Перестановка i-товой строки в место поледней непереставленной
	     (setf row_ie (matr-get-row matr ie) ; Последняя непереставленная строка
		   matr (matr-set-row matr i row_ie) 
		   matr (matr-set-row matr ie row_i)
		   ie (1- ie)) ; Увеличение количества переставленных строк
	     (decf i)) ; Уменьшение переменной цикла для выполнения повторной итерации
	    ((/= matr_ij 0)
	     (setf row_i (mapcar #'(lambda (el) (/ el matr_ij)) row_i) ; Деление строки на matr_ij элемент матрицы
		   matr (matr-set-row matr i row_i)))))
    (setf row_j (matr-get-row matr j)) ; Строка которую необходимо вычесть из других строк
    (do ((i (1+ j)(1+ i))) ; Цикл для вычитания j-товой строки из i-товой
	((> i ie))			
      (setf row_i (matr-get-row matr i)
	    row_i (mapcar (function (lambda (el1 el2) (- el1 el2))) row_i row_j)
	    matr  (matr-set-row matr i row_i))
      (matr-print matr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun matr-obrhod (matr)
  "Обратный ход при вычислении решения системы линейных уравнений;
Матрица matr должна быть приведена к треуголной;"
  (let* ((n (matr-rows matr)) ; Количество строк в матрице (матрица расширенная)
	 (x (matr-new 1 n))) ; Вектор результат
    (do ((i 0 (+ 1 i)))
	((>= i n) x)
      (setf x (matr-set_ij x 1 0 i)))
    (do ((i (- n 1) (- i 1)) (summ 0 0))
	((< i 0) x)
      (do ((j (+ 1 i) (+ 1 j)))
	  ((>= j  n) 'done2)
	(setf summ (+ summ (* (matr-ij matr i j) (matr-ij x 0 j)))))
      (setf x (matr-set_ij x (/ (- (matr-ij matr i n) summ) (matr-ij matr i i)) 0 i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun matr-las-gauss (matr)
  "Решение системы линейных уравнений методом Гаусса
Выводит матрицу с корнями системы линейных уравений коэффициентов
Примеры использования:
Пример 1:
 (matr-print 
  (matr-las-gauss
   (matr-new 3 4 '(1 2 3 14 
		   2 1 1 7 
		   3 0 1 2))))
;=>Matr 1 3
;(1/3 16/3 1)
;
Пример 1.1:
 (matr-print 
  (matr-las-gauss
   (matr-new 3 4 '(1.0 2 3 14 
		   2 1 1 7 
		   3 0 1 2))))
;=>Matr 1 3
;(0.33333397 5.333332 1.0000007)
;
Пример 2:
 (matr-print
  (matr-las-gauss 
   (matr-new 3 4 '(1 0 1 4 
		   0 1 0 2 
		   0 0 1 3))))
;=>Matr 1 3
;(1 2 3)
"
  (let ((matr-tr (matr-triang matr))
	(x nil))
    (format nil "~%Матрица приведенная к тругольной:~%")
    (matr-print matr-tr)
    (format nil "~%Корни системы уравнений:~%")
    (setf x (matr-obrhod matr-tr))
    (matr-print x)
    x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun matr-osr-func (vv ff ex_pts func-name)
  "
Пример использования:
(matr-osr-func '(xx yy) 
	       '((xx xx) (xx) (1.0) (yy)) 
	       '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0))
	       'coool-func)
;
=>(\"Matr\" 1 3 ((0 . 1.0d0) (1 . 0.0d0) (2 . 0.0d0)))
"
  (let ((kk (cons '+ (mapcar #'(lambda(el1 el2) (cons '* (cons el1 el2)))
			     (matr-to-point 
			      (matr-las-gauss
			       (matr-mnk vv ff ex_pts)))
			     ff)))
	(rez nil))
    (setf rez (list 'defun func-name (reverse (cdr(reverse vv))) kk))
    rez))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
