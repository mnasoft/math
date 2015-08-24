(defun matr-name ()
  "Возвращает сторку \"Matr\""
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
(format nil "Ошибка при доступе к эл. матрицы: M[i=~A,j=~A] M[n=~A,m=~A]" i j n m))

(defun matr-ij (matr i j)
  "Доступ к ij элементу матрицы matr элементы начинаются с нуля
Пример использования:
(matr-ij '(\"Matr\" 2 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0) (3 . 4.0) (4 . 5.0) (5 . 6.0)))
0 2
)
"
  (let ((n  (matr-row matr))
	(m  (matr-col matr))
	(li (matr-elements matr)))
  (cond
    ( (or (< i 0) (>= i n))
     (break "~A" (matr-ind_err i j n m)))
    ( (or (< j 0) (>= j m))
     (break "~A" (matr-ind_err i j n m)))
    (t (cdr (assoc (matr-idx i j m) li))))))

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

(defun matr-set_col(matr j pts)
  (do ((n (matr-row matr))
        (m (matr-col matr))
	(i 0))
  ((>= i n) matr)
    (setf matr (matr-set_ij matr (nth i pts)i j))))

(defun matr-get_col (matr j / i n m pts)
  (do ((n (matr-row matr))
       (m (matr-col matr))
       (i 0)
       (pts nil))
  ((>= i n)(reverse pts))
    (setf pts (cons (matr-ij matr i j) pts))))

(defun matr-new	(n m)
"Создание матрицы размером n-строк на m-столбцов
Пример использования:
(matr-new 2 3)
"
(do
 ((li nil)
  (size (* n m))
  (i 0 (1+ i)))
 ((>= i size) (list (matr-name) n m (reverse li)))
  (setf li (cons (cons i 0.0) li))))

(defun matr-new (n m &optional (lst nil))
"Создание матрицы размером n-строк на m-столбцов
Элементы матрицы построчно инициализируются элементами из списка lst
Если элементов в списке недостаточно элементы матрицы инициализируются нулями  
Пример использования:
(matr-new 2 3)
(matr-new 2 3 '(11. 12.))
"
  (do
   ((li nil)
    (size (* n m))
    (i 0 (1+ i)))
   ((>= i size) (list (matr-name) n m (reverse li)))
    (setf li (cons (cons i (cond ((nth i lst)) (t 0.0))) li))))

(defun matr-eval (matr)
  "
(defparameter *alfa* (/ pi 3))
(matr-eval '("Matr" 2 3
	     ((0 . (sin *alfa*)) (1 . 0.0) (2 . 0.0)
	      (3 . 0.0) (4 . (cos *alfa*)) (5 . 0.0))))
"
  (let ((n  (matr-row matr))
	(m  (matr-col matr))
	(li (matr-elements matr)))
    (setf li (mapcar #'(lambda (el) (cons (car el) (eval (cdr el)))) li))
    (list (matr-name) n m li)))



(defun matr-mult (a b / a_n a_m a_n b_m a_li b_li i j k sum c_li)
  "Перемножение матриц
"
  (setq
   a_n	 (cadr a)
   a_m	 (caddr a)
   a_li (cadddr a)
   b_n	 (cadr b)
   b_m	 (caddr b)
   b_li (cadddr b)
   )
  (cond
    (
     (/= a_m b_n)
     (alert
      (strcat
       "Запрещенные размеры матриц для"
       "\n перемножения: A("
       (itoa a_n)
       " x "
       (itoa a_m)
       ") x B("
       (itoa b_n)
       " x "
       (itoa b_m)
       ")"
       )
      )
     )
    )
  (setq i a_n)
  (repeat a_n
	  (setq i (1- i)
		j b_m
		)
	  (repeat b_m
		  (setq j	(1- j)
			k	a_m
			sum	0.0
			)
		  (repeat a_m
			  (setq k	   (1- k)
				a_el (cdr (assoc (matr-idx i k a_m) a_li))
				b_el (cdr (assoc (matr-idx k j b_m) b_li))
				sum  (+ sum (* a_el b_el))
				)
			  )
		  (setq c_li (cons (cons (matr-idx i j b_m) sum) c_li))
		  )
	  )
  (list (matr-name) a_n b_m c_li))

(defun matr-to-point (matr / n m li)
  "Перевод матрицы в точку
(matr-to-point '(\"Matr\" 1 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0))))
(1.0 2.0 3.0)
"
  (setq
   n  (cadr matr)
   m  (caddr matr)
   li (cadddr matr)
   )
  (mapcar 'cdr li)
  )

(defun point-to-matr (p / i li n)
  "Перевод точки в матрицу:
(point-to-matr '(10.0 11.0 12.0 13.0) )
(\"Matr\" 1 4 ((0 . 10.0) (1 . 11.0) (2 . 12.0) (3 . 13.0)))
"
  (setq
   n (length p)
   i -1
   )
  (list
   (matr-name)
   1
   n
   (mapcar
    (function
     (lambda	(el)
      (cons (setq i (1+ i)) el)
      )
     )
    p
    )
   )
  )

(defun matr-print (matr / i j)
  (setq
    i 0
  )
  (while (< i (matr-row matr))
    (setq j 0)
    (princ "\n")
    (while (< j (matr-col matr))
      (princ (matr-ij matr i j))
      (princ " ")
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )
  (princ)
)

(defun matr-mnk(vv ff ex_pts / mtr n m nv)
  "Формирует точки для расчета коэффициентов по методу наименьших квадратов
vv     - '(xx yy) - список, состоящий имен факторов влияния
         и имени функции отклика;
ff     - '(xx yy) '((xx xx) (xx) (1.0) (yy)) - задает 
         вид функциональной зависимости;
ex_pts - '((-1.0 1.0) (2.0 4.0) (3.0 9.0))  - задает 
         значения факторов влияния и значение функции отклика
"
  (setq
   m	(length ff)
   n	(1- m)
   nv	(length vv)
   mtr	(matr-new n m)
   )
  (mapcar
   (function
    (lambda (el / i j)
     (setq i 0)
     (while (< i nv)
       (set (nth i vv) (nth i el))
       (setq i (1+ i))
       )
     (setq i 0)
     (while (< i n)
       (setq j 0)
       (while (< j m)
	 (setq
	  pr  (apply
	       '*
	       (mapcar
		'eval
		(append (nth i ff) (nth j ff))
		)
	       )
	  mtr (matr-set_ij mtr (+ pr (matr-ij mtr i j)) i j)
	  )
	 (setq j (1+ j))
	 )
       (setq i (1+ i))
       )
     )
    )
   ex_pts
   )
  mtr
  )

(defun matr-triang (matr / n m i j k matr_i_k row_i row_k)
  "Сведение матрицы  к треугольному виду матрицы (Для решения системы ЛУ методом Гаусса)
(matr-triang 
 '(\"Matr\" 3 4 ((0 . 1.0) (1 . 0.0) (2 . 1.0) (3 . 4.0) 
		 (4 . 0.0) (5 . 1.0) (6 . 0.0) (7 . 2.0) 
		 (8 . 0.0) (9 . 0.0) (10 . 1.0) (11 . 3.0))))
(\"Matr\" 3 2 ((0 . 1.0) (1 . 0.0) (2 . 1.0) (3 . 4.0) 
               (4 . 0.0) (5 . 1.0) (6 . 0.0) (7 . 2.0) 
	       (8 . 0.0) (9 . 0.0) (10 . 1.0) (11 . 3.0)))
"
  (setq
   n (matr-row matr)
   m (matr-col matr)
   )
  (setq j 0)
  (while (< j n) ;цикл по столбцам	;
    (setq
     ie (1- n)
     i	 j
     )
    (while (<= i ie) ;цикл по строкам для деления ;
      (setq
       row_i	 (matr-get-row matr i)
       matr_ij  (matr-ij matr i j)
       )
      (cond
	(
	 (= matr_ij 0)
	 (setq
	  row_ie (matr-get-row matr ie)
	  matr	  (matr-set-row matr i row_ie)
	  matr	  (matr-set-row matr ie row_i)
	  ie	  (1- ie)
	  )
	 )
	(
	 (/= matr_ij 0)
	 (setq
	  row_i
	  (mapcar
	   (function
	    (lambda (el)
	     (/ el matr_ij)
	     )
	    )
	   row_i
	   )
	  matr	 (matr-set-row matr i row_i)
	  i	 (1+ i)
	  )
	 )
	)
      )
    (setq row_j (matr-get-row matr j));строка которую необходимо вычесть из других строк ;
    (setq i (1+ j))
    (while (<= i ie) ;цикл по строкам для деления ;
      (setq
       row_i (matr-get-row matr i)
       row_i
       (mapcar
	(function
	 (lambda (el1 el2)
	  (- el1 el2)
	  )
	 )
	row_i
	row_j
	)
       matr  (matr-set-row matr i row_i)
       )
      (setq i (1+ i))
      )
    (setq j (1+ j))
    )
  matr
  )

(defun matr-obrhod (matr / i j m n summ x)
  "Обратный ход при вычислении решения системы линейных уравнений
Матрица matr должна быть приведена к треуголной
"
  (setq
   n (matr-row matr)
   m (matr-col matr)
   x (matr-new 1 n)
   )
  (setq i 0)
  (while (< i n)
    (setq x (matr-set_ij x 1.0 0 i))
    (setq i (1+ i))
    )
  (setq i n)
  (while (<= 0 (setq i (1- i)))
    (setq j (1+ i)
	  summ 0.0
	  )
    (while (< j  n)
      (setq summ (+ summ(* (matr-ij matr i j) (matr-ij x 0 j))))
      (setq j (1+ j))
      )
    (setq x (matr-set_ij x (/ (- (matr-ij matr i n) summ)(matr-ij matr i i)) 0 i))
    )
  x
  )

(defun matr-sys_lu(matr / x)
  "Решение системы линейных уравнений методом Гаусса
Выводит матрицу с корнями системы линейных уравений коэффициентов

Например:
(matr-sys_lu  
 '(\"Matr\" 3 4 ((0 . 2.0) (1 . 0.0) (2 . 2.0) (3 . 8.0) 
		 (4 . 0.0) (5 . 2.0) (6 . 0.0) (7 . 4.0) 
		 (8 . 2.0) (9 . 0.0) (10 . 3.0) (11 . 11.0))))

Матрица, приведенная к тругольной:
1.0 0.0 1.0 4.0 
0.0 1.0 0.0 2.0 
0.0 0.0 1.0 3.0
 
Корни системы уравнения:
1.0 2.0 3.0
(\"Matr\" 1 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0)))
"
  (setq matr (matr-triang matr)	)
  (princ "\nМатрица приведенная к тругольной:")
  (matr-print matr)
  (princ "\nКорни системы уравнений:")
  (setq x(matr-obrhod matr))
  (matr-print x)
  x
)
