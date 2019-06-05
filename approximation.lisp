;;;; approximation.lisp

(in-package #:math)

(defun appr_table (x table)
  "Выполняет линейную интерполяцию и экстраполяцию для значения x на таблице table
Пример использования:
(appr_table 0.5 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0)))
(appr_table 1.5 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0)))
(appr_table 3.0 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0)))
Тест: (test-approximation-appr_table)
"
  (labels ((appr-line( XX PP0 PP1)
	     (multiple-value-bind (x p0 p1)
		 (values XX PP0 PP1)
	       (+ (second p0)
		  (/ (*
		      (- x (first p0))
		      (- (second p1) (second p0)))
		     (- (first p1) (first p0))))))
	   (appr-line-list( XX n0 n1 lst)
	     (appr-line
	      XX
	      (nth n0 lst)
	      (nth n1 lst))))
    (do ((len (length table))
	 (res nil)
	 (i 0 (1+ i))
	 )
	((>= i len) (appr-line-list
		     x
		     (first res)
		     (second res )
		     table))
      (cond
	((and (null res) (<= x (first (nth 0 table))))
	 (setf res '(0 1)))
	((and (null res) (<= (first (nth (1- len) table)) x))
	 (setf res (list (- len 2) (- len 1))))
	((and (null res)
	      (<= (first(nth i table)) x)
	      (<= x (first(nth (1+ i) table))))
	 (setf res (list i (1+ i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Разработка линейной интерполяции функции одного переменного
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(export '*apr-args-1*)
(defparameter *apr-args-1* '(x1 yy)
  "Шаблон для построения линейной функции одного параметра: x1 с двумя коэффициентами.")

(export '*apr-func-1-2*)
(defparameter *apr-func-1-2*
  '((x1) (1.0) (yy))
  "Шаблон для построения линейной функции одного параметра: x1 с двумя коэффициентами.")

(export '*apr-func-1-3*)
(defparameter *apr-func-1-3*
  '((x1 x1) (x1) (1.0) (yy))
  "Шаблон для построения квадратичной функции одного параметра: x1 c тремя коэффициентами.")

(export '*apr-func-1-4*)
(defparameter *apr-func-1-4*
  '((x1 x1 x1) (x1 x1) (x1) (1.0) (yy))
  "Шаблон для построения квадратичной функции одного параметра: x1 c четырьмя коэффициентами.")

(export '*apr-func-1-5*)
(defparameter *apr-func-1-5*
  '((x1 x1 x1 x1) (x1 x1 x1) (x1 x1) (x1) (1.0) (yy))
  "Шаблон для построения квадратичной функции одного параметра: x1 c пятью коэффициентами.")

(defun make-linear-interpolation (points &key (ff *apr-func-1-2*))
  "Интерполяция функцией одного переменного зависимости, представленной списком точек.
Примеры использования см. (test-approximation-make-linear-interpolation)
"
  (eval (matr-osr-lambda* '(x1 yy) ff points)))

(defun make-linear-approximation-array (x1 a1d )
  (let ((a1-rez (make-array (mapcar #'1- (array-dimensions a1d)) :initial-element nil)))
    (loop :for r :from 0 :below (array-dimension a1-rez 0) :do
	 (setf (aref a1-rez r)
	       (make-linear-interpolation
		(list
		 (list (aref x1 r)      (aref a1d     r))
		 (list (aref x1 (1+ r)) (aref a1d (1+ r))))
		:ff *apr-func-1-2*)))
    a1-rez))

(defun index-by-value (val vect)
  (let ((rez+ 0)
	(rez- (- (array-dimension vect 0) 2)))
    (cond
      ((< (svref vect 0) (svref vect (1- (array-dimension vect 0))))
       (loop :for i :from 1 :below (1- (array-dimension vect 0)) :do
	    (when (<= (svref vect i) val ) (setf rez+ i)))
       rez+)
      ((> (svref vect 0) (svref vect (1- (array-dimension vect 0))))
       (loop :for i :from (1- (array-dimension vect 0)) :downto 1 :do
	    (when (>= val (svref vect i)) (setf rez- (1- i))))
       rez-))))

(defun approximation-linear (x1 a-x1 a-func)
  (funcall (aref a-func (index-by-value x1 a-x1)) x1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass appr-linear ()
  ((x1       :accessor appr-linear-x1)
   (a1d-func :accessor appr-linear-a1d-func)))

(defmethod print-object ((a-l appr-linear) s)
  (format s "#appr-linear (~A ~A)"
	  (appr-linear-x1       a-l)
	  (appr-linear-a1d-func a-l)))

(defmethod initialize-instance ((a-l appr-linear) &key (x1 (vector -2 -1 0 1 2)) (a1d (vector 4 1 0 1 4)))
  (setf (appr-linear-x1       a-l)
	x1
	(appr-linear-a1d-func a-l)
	(make-linear-approximation-array x1 a1d)))


(defmethod approximate ((point number) (a-l appr-linear))
  (approximation-linear point (appr-linear-x1 a-l) (appr-linear-a1d-func a-l)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Линейная интерполяции функции двух переменных (билинейная интерполяция)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *apr-func-2-4*
  '((x1 x2) (x1) (x2) (1.0) (yy))
  "Шаблон для построения функции двух параметров: x1 и x2 c четырьмя коэффициентами.")

(defun make-belinear-interpolation (points &key (ff *apr-func-2-4*))
  "Создает объект билинейной интерполяции
Пример использования:
  (make-belinear-interpolation
       '((0.0 0.0 10.0)
	 (0.0 1.0 2.0)
	 (1.0 0.0 5.0)
	 (1.0 1.0 15.0)))
"
  (eval (matr-osr-lambda* '(x1 x2 yy) ff points)))

(defun make-bilinear-approximation-array (a2d x1 x2)
  "Генерирует массив, содержащий в своих элементах функции двух переменных.
Функции представляют билинейную интерполяцию массива a2d.
Результирующий массив имеет размерности на единицу меньшую по всем направлениям (строкам и столбцам).
|-------+---+----------+----------+----------+----------+----------|
|       |   | x2[0]    | x2[1]    | x2[2]    | x2[3]    | x2[4]    |
|-------+---+----------+----------+----------+----------+----------|
|-------+---+----------+----------+----------+----------+----------|
| x1[0] |   | a2d[0,0] | a2d[0,1] | a2d[0,2] | a2d[0,3] | a2d[0,4] |
| x1[1] |   | a2d[1,0] | a2d[1,1] | a2d[1,2] | a2d[1,3] | a2d[1,4] |
| x1[2] |   | a2d[2,0] | a2d[2,1] | a2d[2,2] | a2d[2,3] | a2d[2,4] |
| x1[3] |   | a2d[3,0] | a2d[3,1] | a2d[3,2] | a2d[3,3] | a2d[3,4] |
| x1[4] |   | a2d[4,0] | a2d[4,1] | a2d[4,2] | a2d[4,3] | a2d[4,4] |

"
  (when (/= 2 (array-rank a2d)) (error "In make-bilinear-approximation-array: (/= 2 (array-rank a2d))"))
  (let ((a2-rez (make-array (mapcar #'1- (array-dimensions a2d)) :initial-element nil)))
    (loop :for r :from 0 :below (array-dimension a2-rez 0) :do
	 (loop :for c :from 0 :below (array-dimension a2-rez 1) :do
	      (setf (aref a2-rez r c)
		    (make-belinear-interpolation
		     (list
		      (list (svref x1 r)      (svref x2 c)      (aref a2d r          c))
		      (list (svref x1 r)      (svref x2 (1+ c)) (aref a2d r      (1+ c)))
		      (list (svref x1 (1+ r)) (svref x2 c)      (aref a2d (1+ r)     c))
		      (list (svref x1 (1+ r)) (svref x2 (1+ c)) (aref a2d (1+ r) (1+ c))))
		     :ff *apr-func-2-4*))))
    a2-rez))

(defun approximation-bilinear (v1 v2 a-x1 a-x2 a-func)
  (funcall (aref a-func (index-by-value v1 a-x1) (index-by-value v2 a-x2)) v1 v2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass appr-bilinear ()
  ((x1       :accessor appr-bilinear-x1)
   (x2       :accessor appr-bilinear-x2)
   (a2d-func :accessor appr-bilinear-a2d-func)))

(defmethod print-object ((a-l appr-bilinear) s)
  (format s "#appr-bilinear (~A ~A ~A)"
	  (appr-bilinear-x1       a-l)
 	  (appr-bilinear-x2       a-l)
	  (appr-bilinear-a2d-func a-l)))

(defmethod initialize-instance ((a-l appr-bilinear) &key (x1 (vector -1 0 1 )) (x2 (vector 0 1 )) (a2d '#2A((1 2) (3 4) (5 6))))
  (setf (appr-bilinear-x1       a-l) x1
	(appr-bilinear-x2       a-l) x2
	(appr-bilinear-a2d-func a-l) (make-bilinear-approximation-array  a2d x1 x2)))

(defmethod approximate (point (a-l appr-bilinear))
  (approximation-bilinear (aref point 0) (aref point 1) (appr-bilinear-x1 a-l) (appr-bilinear-x2 a-l) (appr-bilinear-a2d-func a-l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Сглаживание методами gnuplot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod make-points-values ((table-values array) (row-values vector) (col-values vector))
  "Формирует массив точек и массив значений
Массив точек    - в строках содержит аргументы функции.
Массив значений - в строках содержит значения функции.
Строки массива точек соответствуют строкам массива значений.
  Тестирование:
 (make-points-values *a* *h-r* *x-r*)
"
  (assert (= (array-rank table-values) 2))
  (assert (= (array-dimension table-values 0) (length row-values)))
  (assert (= (array-dimension table-values 1) (length col-values)))
  (let* ((dims (array-dimensions table-values))
	 (len  (apply #'* dims))
	 (cols (second dims))
	 (points (make-array (list len 2 ) :initial-element 0.0))
	 (values (make-array (list len   ) :initial-element 1.0)))
    (loop :for i :from 0 :below (array-dimension table-values 0) :do
	 (loop :for j :from 0 :below (array-dimension table-values 1) :do
	      (setf (aref  points (+ j (* i cols)) 0) (svref row-values  i)
		    (aref  points (+ j (* i cols)) 1) (svref col-values j)
		    (svref values (+ j (* i cols))  ) (aref  table-values i j))))
    (values points values)))


(export 'approx-by-points)
(defgeneric approx-by-points (pnt d-pnt points values &key w-func)
  (:documentation
   "Вычисляет функцию, заданную точками points и значениями values
в точке pnt, используя размер влияния, заданный параметром d-pnt.
"))

(defmethod approx-by-points ((x number) (dx number) (points vector) (values vector) &key (w-func #'gauss-smoothing))
  "Пример использования:
 (approx-by-points 1.0 0.6 (vector 0.0 1.0 2.0 3.0 4.0) (vector 0.0 1.0 2.0 3.0 4.0)) 1.0000265
 (approx-by-points 1.0 0.4 (vector 0.0 1.0 2.0 3.0 4.0) (vector 0.0 1.0 2.0 3.0 4.0)) 1.0
 (approx-by-points 1.0 0.8 (vector 0.0 1.0 2.0 3.0 4.0) (vector 0.0 1.0 2.0 3.0 4.0)) 1.0027183
 (approx-by-points 1.0 1.0 (vector 0.0 1.0 2.0 3.0 4.0) (vector 0.0 1.0 2.0 3.0 4.0)) 1.0210931
 (approx-by-points 1.0 1.5 (vector 0.0 1.0 2.0 3.0 4.0) (vector 0.0 1.0 2.0 3.0 4.0)) 1.1591185
"
  (assert (= (length points) (length values)))
  (let ((w-summ 0.0)
	(w-z-summ 0.0)
	(w 0.0)
	(z 0.0))
    (loop :for i :from 0 :below  (array-dimension points 0) :do
	 (progn
	   (setf w (funcall w-func (distance-relative x (aref points i) dx))
		 w-summ (+ w-summ w)
		 z (aref values i)
		 w-z-summ (+ w-z-summ (* w z )))))
    (/ w-z-summ w-summ)))



(defmethod approx-by-points ((x vector) (dx vector) (points array) (values vector) &key (w-func #'gauss-smoothing))
  "Пример использования:
 
 (approx-by-points (vector 0.0 0.0) (vector 1.0 1.0) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 0.53788286
 (approx-by-points (vector 0.0 0.0) (vector 0.6 0.6) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 0.117073804
 (approx-by-points (vector 0.0 0.0) (vector 0.4 0.4) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 0.0038534694

 (approx-by-points (vector 1.0 0.0) (vector 1.0 1.0) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 1.0
 (approx-by-points (vector 1.0 0.0) (vector 0.6 0.6) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 0.9999999
 (approx-by-points (vector 1.0 0.0) (vector 0.4 0.4) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 1.0

 (approx-by-points (vector 0.0 1.0) (vector 1.0 1.0) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 1.0
 (approx-by-points (vector 0.0 1.0) (vector 0.6 0.6) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 1.0
 (approx-by-points (vector 0.0 1.0) (vector 0.4 0.4) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 1.0

 (approx-by-points (vector 1.0 1.0) (vector 1.0 1.0) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 1.4621171
 (approx-by-points (vector 1.0 1.0) (vector 0.6 0.6) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 1.8829262
 (approx-by-points (vector 1.0 1.0) (vector 0.4 0.4) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 1.9961466

 (approx-by-points (vector 0.5 0.5) (vector 1.0 1.0) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 1.0
 (approx-by-points (vector 0.5 0.5) (vector 0.6 0.6) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 1.0
 (approx-by-points (vector 0.5 0.5) (vector 0.4 0.4) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0)) 1.0
"
  (assert (= (array-rank points) 2))
  (assert (= (array-dimension points 0) (length values)))
  (assert (= (array-dimension points 1) (length x) (length dx)))
  (let ((w-summ 0.0)
	(w-z-summ 0.0)
	(w 0.0)
	(z 0.0))
    (loop :for i :from 0 :below  (array-dimension points 0) :do
	 (progn
	   (setf w (funcall w-func (distance-relative x (row i points) dx))
		 w-summ (+ w-summ w)
		 z (aref values i)
		 w-z-summ (+ w-z-summ (* w z )))))
    (/ w-z-summ w-summ)))
