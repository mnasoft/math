;;;; matr.lisp

(in-package #:math)

(declaim (optimize (debug 3)))

(defun matr-name ()
  "Возвращает сторку \"Matr\"
;;;;
Пример использования:
(matr-name)
"
  "Matr")

(defun matr-idx (i j m)
  "Вычисляет индекс (i,j) элемента матрицы индексы начинаются с нуля
m-количест во столбцов в матрице"
  (+ (* i m) j))

(defun matr-row (matr)
  "Возвращает количество строк в матрице
(matr-row '(\"Matr\" 2 3 ((0 . 0.0) (1 . 0.0) (2 . 0.0) (3 . 0.0) (4 . 0.0) (5 . 0.0))))
2" 
  (cadr matr))

(defun matr-col (matr)
"Возвращает количество столбцов в матрице
(matr-col '(\"Matr\" 2 3 ((0 . 0.0) (1 . 0.0) (2 . 0.0) (3 . 0.0) (4 . 0.0) (5 . 0.0))))
3"
 (caddr matr))

(defun matr-elements (matr)
"Возвращает количество столбцов в матрице
Пример использования:
(matr-col '(\"Matr\" 2 3 ((0 . 0.0) (1 . 0.0) (2 . 0.0) (3 . 0.0) (4 . 0.0) (5 . 0.0))))
3"
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
  (let ((n  (matr-row matr))
	(m  (matr-col matr)))
    (if (not (and(in-range-inc-exc i 0 n) (in-range-inc-exc j 0 m)))
	(break "Ошибка при доступе к эл. матрицы: M[~A,~A] M[0..~A,0..~A]" i j (- n 1) (-  m 1 )))))


(defun matr-ij (matr i j)
  "Доступ к i,j элементу матрицы matr
Нумерация элементов начинается с нуля
Пример использования:
(matr-ij '(\"Matr\" 2 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0) (3 . 4.0) (4 . 5.0) (5 . 6.0)))
	 0 2)
"
  (let ((n  (matr-row matr))
	(m  (matr-col matr))
	(li (matr-elements matr)))
    (matr-index-is-good matr i j)
    (cdr (assoc (matr-idx i j m) li))))

(defun matr-set_ij (matr elem i j)
  "Присвоение значения elem элементу матрицы matr(i j) элементы начинаются с нуля
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

(defun matr-set-row(matr i pts)
  "Пример использования:
(matr-set-row
 '(\"Matr\" 2 3 ((0 . 0.0) (1 . 0.0) (2 . 0.0) 
		 (3 . 0.0) (4 . 0.0) (5 . 0.0)))
 0 '(11 12 13))
"
  (do ((n (matr-row matr))
       (m (matr-col matr))
       (j 0 (1+ j)))
      ( (>= j m) matr)
    (setf matr (matr-set_ij matr (nth j pts)i j))))

(defun matr-get-row (matr i)
  "Пример использования:
(matr-get-row
 '(\"Matr\" 2 3 ((0 . 0.0) (1 . 0.0) (2 . 0.0) 
		 (3 . 0.0) (4 . 0.0) (5 . 0.0)))
 0)
"
  (do ((n (matr-row matr))
	(m (matr-col matr))
	(j 0 (1+ j))
	(pts nil))
      ((>= j m) (reverse pts))
    (setf pts (cons (matr-ij matr i j) pts))))

(defun matr-set-col(matr j pts)
  "Пример использования;
(matr-set-col
 '(\"Matr\" 2 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0) 
		 (3 . 4.0) (4 . 5.0) (5 . 6.0)))
 0 '(11.0 12.0))"
  (do ((n (matr-row matr))
       (m (matr-col matr))
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
  (do ((n (matr-row matr))
       (m (matr-col matr))
       (i 0 (1+ i))
       (pts nil))
      ((>= i n)(reverse pts))
    (setf pts (cons (matr-ij matr i j) pts))))

(defun matr-new (n m &optional (lst nil))
"Создание матрицы размером n-строк на m-столбцов
Элементы матрицы построчно инициализируются элементами из списка lst
Если элементов в списке недостаточно элементы матрицы инициализируются нулями  
Пример использования:
(matr-new 2 3)
(matr-new 2 3 '(11.0d0 12.0d0))
"
  (do
   ((li nil)
    (size (* n m))
    (i 0 (1+ i)))
   ((>= i size) (list (matr-name) n m (reverse li)))
    (setf li (cons (cons i (cond ((nth i lst)) (t 0.0d0))) li))))

(defun matr-eval (matr)
  "Выполняет поэлементное оценивание каждого элемента матрицы
Пример использования:
(defparameter *alfa* (/ pi 3))
(matr-eval '(\"Matr\" 2 3
	     ((0 . (sin *alfa*)) (1 . 0.0) (2 . 0.0)
	      (3 . 0.0) (4 . (cos *alfa*)) (5 . 0.0))))
"
  (let ((n  (matr-row matr))
	(m  (matr-col matr))
	(li (matr-elements matr)))
    (setf li (mapcar #'(lambda (el) (cons (car el) (eval (cdr el)))) li))
    (list (matr-name) n m li)))

(defun matr-mult (a b)
  "Перемножение матриц a и b
(matr-mult  '(\"Matr\" 2 3
	     ((0 . 1.0) (1 . 2.0) (2 . 3.0)
	      (3 . 4.0) (4 . 5.0) (5 . 6.0)))
	     '(\"Matr\" 3 2
	       ((0 . 1.0) (1 . 2.0)
		(2 . 3.0) (3 . 4.0)
		(4 . 5.0) (5 . 6.0)))
	     )
"
  (let ((a_n (matr-row a))
	(a_m (matr-col a))
	(b_n (matr-row b))
	(b_m (matr-col b))
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

(defun matr-to-point (matr)
  "Перевод матрицы в точку
(matr-to-point '(\"Matr\" 1 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0))))
"
  (let ((n  (matr-row matr))
	(m  (matr-col matr))
	(li (matr-elements matr)))
    (mapcar #'cdr li)))

(defun point-to-matr (p)
  "Перевод точки в матрицу
;;;;
Пример использования:
(point-to-matr '(10.0 11.0 12.0 13.0))
"
  (matr-new 1 (length p) p))

(defun matr-to-string (matr)
  "Выводит представление матрицы в виде строки символов
;;;;
Пример использования:
(matr-to-string '(\"Matr\" 2 3
	     ((0 . (sin *alfa*)) (1 . 0.0) (2 . 0.0)
	      (3 . 0.0) (4 . (cos *alfa*)) (5 . 0.0))))
"
  (let ((n (matr-row matr))
	(m (matr-col matr))
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
;;;;
Пример использования;
(matr-mnk '(xx yy) '((xx xx) (xx) (1.0) (yy)) '((-1.0 1.0) (2.0 4.0) (3.0 9.0)))
"
  (let* ((m (length ff))
	 (n (1- m))
	 (nv (length vv))
	 (mtr (matr-new n m)))
    (mapcar
     #'(lambda (el)
	 (do ((i 0 (1+ i)))
	     ((>= i nv))
	   (setf (nth i vv) (nth i el)))
	 (do ((i 0 (1+ i)))
	     ((>= i n))
	   (do ((j 0 (1+ j))
		(pr nil))
	       ((>= j m))
	     (setf pr (apply #'* (mapcar #'eval (append (nth i ff) (nth j ff))))
		   mtr (matr-set_ij mtr (+ pr (matr-ij mtr i j)) i j)))))
     ex_pts)
    mtr))

(defun matr-triang (matr)
  "Сведение матрицы  к треугольному виду матрицы (Для решения системы ЛУ методом Гаусса)
(matr-triang 
 '(\"Matr\" 3 4 ((0 . 1.0d0) (1 . 0.0d0) (2  . 1.0d0) (3  . 4.0d0) 
		 (4 . 0.0d0) (5 . 1.0d0) (6  . 0.0d0) (7  . 2.0d0) 
		 (8 . 0.0d0) (9 . 0.0d0) (10 . 1.0d0) (11 . 3.0d0))))
n - количество сторк;
m - количество столбцов;
"
  (matr-print matr)
  (do ((n (matr-row matr)) (m (matr-col matr)) (ie nil) (j 0 (1+ j)) (row_j nil) (row_i nil))
      ( (>= j n) matr)
    (setf ie (1- n))
    (do (  (i j (1+ i)) (matr_ij nil) (row_ie nil))
	( (> i ie))
      (setf row_i   (matr-get-row matr i)
	    matr_ij (matr-ij matr i j))
      (cond
	((= matr_ij 0)
	 (setf row_ie (matr-get-row matr ie)
	       matr (matr-set-row matr i row_ie)
	       matr (matr-set-row matr ie row_i)
	       ie (1- ie)))
	((/= matr_ij 0)
	 (setf row_i (mapcar #'(lambda (el) (/ el matr_ij)) row_i)
	       matr (matr-set-row matr i row_i))))
      (matr-print matr))
    (setf row_j (matr-get-row matr j)) ;строка которую необходимо вычесть из других строк ;
    (do ((i (1+ j)(1+ i)))
	((> i ie))			;цикл по строкам для деления ;
      (setf row_i (matr-get-row matr i)
	    row_i (mapcar (function (lambda (el1 el2) (- el1 el2))) row_i row_j)
	    matr  (matr-set-row matr i row_i))
      (matr-print matr))))

(defun matr-obrhod (matr)
  "Обратный ход при вычислении решения системы линейных уравнений
Матрица matr должна быть приведена к треуголной
"
  (let* ((n (matr-row matr))
	 (m (matr-col matr))
	 (x (matr-new 1 n)))
;;;;    (break "00~%" (matr-print x))
    (do ((i 0 (+ 1 i)))
	((>= i n) x)
      (setf x (matr-set_ij x 1.0 0 i)))
;;;;    (break "10~%~A~%" (matr-print x))
    (do ((i (- n 1)(- i 1))
	 (summ 0.0))
	((< i 0) x)
;;;;      (break "20~%")
      (do ((j (+ 1 i) (+ 1 j)))
	  ((>= j  n) 'done2)
	(setf summ (+ summ(* (matr-ij matr i j) (matr-ij x 0 j))))
;;;;	(break "30~%")
	)
;;;;      (break "40~%")
      (setf x (matr-set_ij x (/ (- (matr-ij matr i n) summ)(matr-ij matr i i)) 0 i)))
    ))
 
(defun matr-sys_lu(matr)
  "Решение системы линейных уравнений методом Гаусса
Выводит матрицу с корнями системы линейных уравений коэффициентов

Например:
(matr-sys_lu  
 '(\"Matr\" 3 4 
   ((0 . 10.0d0) (1 . 2.0d0) (2  .  3.0d0) (3  . 4.0d0) 
    (4 . 40.0d0) (5 . 5.0d0) (6  .  6.0d0) (7  . 2.0d0) 
    (8 . 70.0d0) (9 . 8.0d0) (10 . 10.0d0) (11 . 3.0d0))))

Матрица, приведенная к тругольной:
1.0 0.0 1.0 4.0 
0.0 1.0 0.0 2.0 
0.0 0.0 1.0 3.0
 
Корни системы уравнения:
1.0 2.0 3.0
(\"Matr\" 1 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0)))
"
(let ((matr-tr (matr-triang matr))
      (x nil))
  (format nil "~%Матрица приведенная к тругольной:~%")
  (matr-print matr-tr)
  (format nil "~%Корни системы уравнений:~%")
  (setf x(matr-obrhod matr-tr))
  (matr-print x)
  x))

(matr-sys_lu  
 '(\"Matr\" 3 4 
   ((8 . 0.0d0) (9 . 8.0d0) (10 . 10.0d0) (11 . 3.0d0)
    (4 . 0.0d0) (5 . 5.0d0) (6 . 6.0d0) (7  . 2.0d0) 
    (0 . 100.0d0) (1 . 2.0d0) (2 . 3.0d0) (3  . 4.0d0)
    )))

(matr-triang 
 '("Matr" 3 4 ((0 . 1.0d0) (1 . 2.0d0) (2  . 3.0d0) (3  . 4.0d0) 
	       (4 . 40.0d0) (5 . 5.0d0) (6  . 6.0d0) (7  . 2.0d0) 
	       (8 . 70.0d0) (9 . 8.0d0) (10 . 10.0d0) (11 . 3.0d0))))


(defun matr-rotation (matr)
  "Решение системы линейных алгебраических уравнений (СЛАУ) методом вращения, состоящего из:
- сведения СЛАУ к треугольной системе;
- нахождение корней методом обратного хода метода Гаусса
;
Возвращает вектор с количеством элементов равным количеству строк в СЛАУ
;
Matr - массив, у которого количество строк (первая размерность)
должно быть на единицу меньше количества столбцов (вторая размерность)
Данная матрица меняется в процессе в процессе вычисления функции
;
Пример использования:
(defparameter *m-test*
  (make-array '(3 4)
	      :initial-contents
	      '((10.0d0 11.0d0  12.0d0  4.0d0)
		(15.0d0 17.0d0  21.0d0  2.0d0)
		(70.0 8.0  10.0 3.0))))
(matr-rotation (copy-array *m-test*))
;
Есть необходимость доработки с точки зрения решения разреженной СЛАУ
"
  (let ((n (array-dimension matr 0))	; Количество строк
	(m (array-dimension matr 1))	; Количество столбцов
	)
    (if (/= (1+ n) m)			; Проверка размерности
	(break "ERROR IN FUNC matr-rotation:~%n+1 != m~%" ))
    (do ((i 0 (1+ i)))
	((not (< i n)) matr)
      (do ((a nil) (b nil) (c nil) (s nil) (tmp nil)
	   (j  (1+ i) (1+ j)))
	  ((not (< j n)) 'done-do-02)
	(setf a (aref matr i i)
	      b (aref matr j i)
	      c (/ a (sqrt (+ (* a a) (* b b))))
	      s (/ b (sqrt (+ (* a a) (* b b)))))
	(do ((k i (1+ k )))
	    ((not (<= k n)) 'done-do-03)
	  ;;	  (break "001 i=~A j=~A k=~A~%a=~A b=~A c=~A s=~A~%~S~%" i j k a b c s matr)
	  (setf tmp (aref matr i k)
		(aref matr i k) (+ (* c (aref matr i k)) (* s (aref matr j k)))
		(aref matr j k) (- (* c (aref matr j k)) (* s tmp))))))
    (do ((i (1- n) (1- i))		; Обратный ход метода Гаусса
	 (x (make-array n :initial-element 1.0d0))
	 (summ 0.0d0 0.0d0))
	((not (>= i 0)) x)
      (do ((j (1+ i) (1+ j)))
	  ((not (< j n)))
	(setf summ (+ summ (* (aref matr i j) (aref x j)))))
      (setf summ (- (aref matr i n) summ)
	    (aref x i) (/ summ (aref matr i i)))
      )))

(defun mult-matr-vect (matr vect)
  "Умножение матрицы Matr на вектор Vect
Возвращает вектор с количеством элементов равным количеству элементов в векторе Vect
Пример использования:
(defparameter *m-test*
  (make-array '(3 4)
	      :initial-contents
	      '((10.0d0 11.0d0  12.0d0  4.0d0)
		(15.0d0 17.0d0  21.0d0  2.0d0)
		(70.0 8.0  10.0 3.0))))

(mult-matr-vect  *m-test* ; Проверка правильности решения (системы линейных алгебраических уравнений) СЛАУ
		 (matr-rotation (copy-array *m-test*)))

"
  (let* ((n (array-dimension vect 0))
	 (vect-rez (make-array n :initial-element 0.0d0)))
    (do ((i 0 (1+ i)))
	((= i n) (values vect-rez matr vect ))
      (do ((j 0 (1+ j))
	   (summ 0.0d0))
	  ((= j n) (setf (aref vect-rez i) summ))
	(setf summ (+ summ (* (aref matr i j )
			      (aref vect j))))))))

