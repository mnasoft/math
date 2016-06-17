;;;; matr.lisp

(in-package #:math)

(declaim (optimize (debug 3)))
(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

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

(defun matr-rows (matr)
  "Возвращает количество строк в матрице
(matr-rows '(\"Matr\" 2 3 ((0 . 0.0) (1 . 0.0) (2 . 0.0) (3 . 0.0) (4 . 0.0) (5 . 0.0))))
2" 
  (cadr matr))

(defun matr-cols (matr)
"Возвращает количество столбцов в матрице
(matr-cols '(\"Matr\" 2 3 ((0 . 0.0) (1 . 0.0) (2 . 0.0) (3 . 0.0) (4 . 0.0) (5 . 0.0))))
3"
 (caddr matr))

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


(defun matr-ij (matr i j)
  "Возвращает [i,j] элемент матрицы matr;
Нумерация каждого индекса элементоа начинается с нуля;
Пример использования:
(matr-ij '(\"Matr\" 2 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0) (3 . 4.0) (4 . 5.0) (5 . 6.0)))
	 0 2)
"
  (let ((n  (matr-rows matr))
	(m  (matr-cols matr))
	(li (matr-elements matr)))
    (matr-index-is-good matr i j)
    (cdr (assoc (matr-idx i j m) li))))

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

(defun matr-set-row(matr i pts)
  "Пример использования:
(matr-set-row
 '(\"Matr\" 2 3 ((0 . 0.0) (1 . 0.0) (2 . 0.0) 
		 (3 . 0.0) (4 . 0.0) (5 . 0.0)))
 0 '(11 12 13))
"
  (do ((n (matr-rows matr))
       (m (matr-cols matr))
       (j 0 (1+ j)))
      ( (>= j m) matr)
    (setf matr (matr-set_ij matr (nth j pts)i j))))

(defun matr-get-row (matr i)
  "Пример использования:
(matr-get-row
 '(\"Matr\" 2 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0) 
		 (3 . 11.0) (4 . 12.0) (5 . 13.0)))
 1)
"
  (do ((n (matr-rows matr))
	(m (matr-cols matr))
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
  (do ((n (matr-rows matr))
       (m (matr-cols matr))
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
       (m (matr-cols matr))
       (i 0 (1+ i))
       (pts nil))
      ((>= i n)(reverse pts))
    (setf pts (cons (matr-ij matr i j) pts))))

(defun matr-new (n m &optional (lst nil))
  "Создаёт матрицу размером n-строк на m-столбцов;
Элементы матрицы построчно инициализируются элементами из списка lst;
Если элементов в списке недостаточно элементы матрицы инициализируются нулями;
;
Пример использования:
(matr-new 2 3)
=> (\"Matr\" 2 3
 ((0 . 0.0d0) (1 . 0.0d0) (2 . 0.0d0) (3 . 0.0d0) (4 . 0.0d0) (5 . 0.0d0)))
;
(matr-new 2 3 '(11.0d0 12.0d0))
=> (\"Matr\" 2 3
 ((0 . 11.0d0) (1 . 12.0d0) (2 . 0.0d0) (3 . 0.0d0) (4 . 0.0d0) (5 . 0.0d0)))
"
  (do
   ((li nil)
    (size (* n m))
    (i 0 (1+ i)))
   ((>= i size) (list (matr-name) n m (reverse li)))
    (setf li (cons (cons i (cond ((nth i lst)) (t 0.0d0))) li))))

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

(defun matr-to-point (matr)
  "Выполняет преобразование матрицы в точку
(matr-to-point '(\"Matr\" 1 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0))))
=> (1.0 2.0 3.0)
;
(matr-to-point '(\"Matr\" 3 1 ((0 . 1.0) (1 . 2.0) (2 . 3.0))))
=> (1.0 2.0 3.0)
;
(matr-to-point '(\"Matr\" 3 2 ((0 . 1.0) (1 . 2.0) (2 . 3.0) (3 . 11.0) (4 . 12.0) (5 . 13.0))))
=> (1.0 2.0 3.0 11.0 12.0 13.0)
;
(matr-to-point '(\"Matr\" 2 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0) (3 . 11.0) (4 . 12.0) (5 . 13.0))))
=> (1.0 2.0 3.0 11.0 12.0 13.0)
"
  (let ((n  (matr-rows matr))
	(m  (matr-cols matr))
	(li (matr-elements matr)))
    (mapcar #'cdr li)))

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
  (let* ((m (length ff))
	 (n (1- m))
	 (nv (length vv))
	 (mtr (matr-new n m)))
    (mapcar #'(lambda (el)
		(dotimes (i n)
		  (dotimes (j m)
		    (setf mtr (matr-set_ij mtr (+ (apply (eval (list 'lambda  vv (cons '* (append (nth i ff) (nth j ff))))) el)
						  (matr-ij mtr i j)) i j)))))
	    ex_pts)
    mtr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun matr-triang (matr)		; Старая версия
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
  (do ((n (matr-rows matr)) (m (matr-cols matr)) (ie nil) (j 0 (1+ j)) (row_j nil) (row_i nil))
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

(defun matr-triang-test (matr)		; Отладочная версия ; ; ;
  "Выполняет приведение  матрицы  к треугольному виду, для решения системы ЛУ методом Гаусса;
Пример использования 1
;(matr-triang '(\"Matr\" 3 4 ((0 . 1.0d0) (1 . 0.0d0) (2  . 1.0d0) (3  . 4.0d0) (4 . 0.0d0) (5 . 1.0d0) (6  . 0.0d0) (7  . 2.0d0) (8 . 0.0d0) (9 . 0.0d0) (10 . 1.0d0) (11 . 3.0d0))))
; 
;=> (\"Matr\" 3 4 ((0 . 1.0d0) (1 . 0.0d0)  (2 . 1.0d0) (3 . 4.0d0) (4 . 0.0d0) (5 . 1.0d0)  (6 . 0.0d0) (7 . 2.0d0) (8 . -1.0d0) (9 . 0.0d0) (10 . 0.0d0) (11 . -1.0d0)))
Пример использования 2
(matr-triang '(\"Matr\" 3 4 ((0 . 1.0d0) (1 . 0.0d0) (2  . 1.0d0) (3  . 4.0d0) (4 . 0.0d0) (5 . 1.0d0) (6  . 0.0d0) (7  . 2.0d0) (8 . 0.0d0) (9 . 0.0d0) (10 . 1.0d0) (11 . 3.0d0))))
"
  (let ((m-bak (matr-copy matr))
	(n (matr-rows matr)) (m (matr-cols matr)) (ie nil)
	(row_j nil) (row_i nil)
	(matr_ij nil) (row_ie nil) ;; *0001*
	)
    (do ((j 0 (1+ j)))
	((>= j n) matr)
      (setf ie (1- n))
      (break ":01:(do ((j 0 (1+ j))):~%matr-bak~%~S~%matr-new~%~S~% j=~S ie=~S"
	     (matr-to-string m-bak) (matr-to-string matr) j ie)
      (do ((i j (1+ i)) ) ;; *0001*
	  ((> i ie))
	(setf row_i   (matr-get-row matr i)
	      matr_ij (matr-ij matr i j))
	(break ":02:(do ((i j (1+ i)) (matr_ij nil) (row_ie nil)):~%matr-bak~%~S~%matr-new~%~S~%i=~S j=~S ie=~S~%row_i=~S~%matr_ij=~S"
	       (matr-to-string m-bak) (matr-to-string matr) i j ie row_i matr_ij)
	(cond
	  ((= matr_ij 0)
	   (setf row_ie (matr-get-row matr ie)
		 matr (matr-set-row matr i row_ie)
		 matr (matr-set-row matr ie row_i)
		 ie (1- ie))
	   (decf i)
	   (break ":03:(cond((= matr_ij 0)~%matr-bak~%~S~%matr-new~%~S~%i=~S j=~S ie=~S~%row_i=~S~%matr_ij=~S"
		  (matr-to-string m-bak) (matr-to-string matr) i j ie row_i matr_ij)
	   )
	  ((/= matr_ij 0)
	   (setf row_i (mapcar #'(lambda (el) (/ el matr_ij)) row_i)
		 matr (matr-set-row matr i row_i))
	   (break ":04:(cond (/= matr_ij 0)~%matr-bak~%~S~%matr-new~%~S~%i=~S j=~S ie=~S~%row_i=~S~%matr_ij=~S"
		  (matr-to-string m-bak) (matr-to-string matr) i j ie row_i matr_ij)
	   ))
	(matr-print matr))
      (setf row_j (matr-get-row matr j)) ;; строка которую необходимо вычесть из других строк
      (break ":05:(cond (/= matr_ij 0)~%matr-bak~%~S~%matr-new~%~S~%j=~S ie=~S~%row_i=~S~%row_j=~S~%matr_ij=~S"
	     (matr-to-string m-bak) (matr-to-string matr) j ie row_i row_j matr_ij)
      (do ((i (1+ j)(1+ i)))
	  ((> i ie)) ;; цикл по строкам для деления
	(setf row_i (matr-get-row matr i)
	      row_i (mapcar (function (lambda (el1 el2) (- el1 el2))) row_i row_j)
	      matr  (matr-set-row matr i row_i))
	(break ":06:(cond (/= matr_ij 0)~%matr-bak~%~S~%matr-new~%~S~%j=~S ie=~S row_i=~S~%matr_ij=~S"
	       (matr-to-string m-bak) (matr-to-string matr) j ie row_i matr_ij)
	(matr-print matr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun matr-obrhod (matr)
  "Обратный ход при вычислении решения системы линейных уравнений;
Матрица matr должна быть приведена к треуголной;"
  (let* ((n (matr-rows matr)) ; Количество строк в матрице (матрица расширенная)
	 (m (matr-cols matr)) ; Количество столбцов в матрице (матрица расширенная)
	 (x (matr-new 1 n))) ; Вектор результат
    (do ((i 0 (+ 1 i)))
	((>= i n) x)
      (setf x (matr-set_ij x 1.0 0 i)))
    (do ((i (- n 1) (- i 1)) (summ 0.0 0.0))
	((< i 0) x)
      (do ((j (+ 1 i) (+ 1 j)))
	  ((>= j  n) 'done2)
	(setf summ (+ summ (* (matr-ij matr i j) (matr-ij x 0 j)))))
      (setf x (matr-set_ij x (/ (- (matr-ij matr i n) summ) (matr-ij matr i i)) 0 i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun matr-las-gauss (matr)
  "Решение системы линейных уравнений методом Гаусса
Выводит матрицу с корнями системы линейных уравений коэффициентов
;
Например:
;(matr-las-gauss '(\"Matr\" 3 4 ((0 . 1.0)  (1 . 2.0) (2 . 3.0) (3 . 14.0) 
				 (4 . 2.0)  (5 . 1.0) (6  . 1.0) (7  . 7.0) 
				 (8 . 3.0)  (9 . 0.0) (10 . 1.0) (11 . 2.0))))

;(matr-las-gauss '(\"Matr\" 3 4 ((0 . 1.0d0) (1 . 0.0d0) (2 . 1.0) (3  . 4.0d0) (4 . 0.0d0) (5 . 1.0d0) (6  . 0.0d0) (7  . 2.0d0) (8 . 0.0d0) (9 . 0.0d0) (10 . 1.0d0) (11 . 3.0d0))))
;
Матрица, приведенная к тругольной:
;(\"Matr\" 3 4 ((0 . 1.0) (1 . 2.0) (2 . 3.0d0) (3 . 14.0)(4 . -0.0) (5 . 1.0) (6 . 1.6666666666666667d0) (7 . 7.0) (8 . -0.0d0) (9 . -0.0d0) (10 . 1.0d0) (11 . 3.0d0)))
;
Корни системы уравнения:
;=> 1.0 2.0 3.0
;=> (\"Matr\" 1 3 ((0 . 1.0) (1 . 2.0) (2 . 3.0)))
"
(let ((matr-tr (matr-triang matr))
      (x nil))
  (format nil "~%Матрица приведенная к тругольной:~%")
  (matr-print matr-tr)
  (format nil "~%Корни системы уравнений:~%")
  (setf x (matr-obrhod matr-tr))
  (matr-print x)
  x))

(defun matr-osr-func (vv ff ex_pts func-name)
  (let ((kk (cons 
	     '+ (mapcar 
		 #'(lambda(el1 el2) (cons '* (cons el1 el2)))
		 (math:matr-to-point 
		  (math:matr-las-gauss
		   (math:matr-mnk vv ff ex_pts)))
		 ff))))
(list 'defun func-name (reverse (cdr(reverse vv))) kk)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(let ((m-test (make-array '(3 4)
			  :initial-contents
			  ' ((1.0d0 0.0d0 1.0d0 4.0d0) 
			     (0.0d0 1.0d0 0.0d0 2.0d0) 
			     (0.0d0 0.0d0 1.0d0 3.0d0))
			  )))
  (matr-rotation (copy-array m-test)))
=> #(1.0d0 2.0d0 3.0d0)
;
(let ((m-test (make-array '(3 4)
			  :initial-contents
			  '((10.0d0 11.0d0  12.0d0  4.0d0)
			    (15.0d0 17.0d0  21.0d0  2.0d0)
			    (70.0 8.0  10.0 3.0))
			  )))
  (matr-rotation (copy-array m-test)))
=>#(0.03588235294117642d0 2.182352941176469d0 -1.6970588235294102d0)
;
Есть необходимость доработки с точки зрения решения разреженной СЛАУ?
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
;
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


   
