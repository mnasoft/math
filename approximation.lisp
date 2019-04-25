;;;; approximation.lisp

(in-package #:math)

(defun appr-line( XX PP0 PP1)
  (multiple-value-bind (x p0 p1)
      (values XX PP0 PP1)
    (+ (second p0)
       (/ (*
	   (- x (first p0))
	   (- (second p1) (second p0)))
	  (- (first p1) (first p0))))))

(defun appr-line-list( XX n0 n1 lst)
  (appr-line
   XX
   (nth n0 lst)
   (nth n1 lst)))

(defun appr_table (x table)
  "
Выполняет линейную интерполяцию и экстраполяцию для значения x на таблице table
Пример использования:
(appr_table 0.5 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0)))
(appr_table 1.5 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0)))
(appr_table 3.0 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0)))
"
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
       (setf res (list i (1+ i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Разработка
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *apr-func-4*
  '((x1 x2) (x1) (x2) (1.0) (yy))
  "Шаблон для построения функции двух параметров: x1 и x2 c четырьмя коэффициентами.")

(defun make-belinear-interpolation (points &key (ff *apr-func-4*))
  "Создает объект билинейной интерполяции
Пример использования:
  (make-belinear-interpolation
       '((0.0 0.0 10.0)
	 (0.0 1.0 2.0)
	 (1.0 0.0 5.0)
	 (1.0 1.0 15.0)))
"
  (eval (matr-osr-lambda '(x1 x2 yy) ff points)))

(defun make-approximation-array-bilinear (a2d x1 x2)
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
  (when (/= 2 (array-rank a2d)) (error "In make-approximation-array-bilinear: (/= 2 (array-rank a2d))"))
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
		     :ff *apr-func-4*))))
    a2-rez))

(defun approximation-bilinear (v1 v2 a-x1 a-x2 a-func)
  (labels ((index-by-value (val vect)
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
		  rez-)))))
    (funcall (aref a-func (index-by-value v1 a-x1) (index-by-value v2 a-x2)) v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Тестирование
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *test-data-01*
    '((117.0 120.1 118.6 112.5 115.0)
      (115.0 125.2 119.7 122.4 112.0)
      (113.0 135.3 129.8 132.3 117.0)
      (112.0 122.4 124.9 122.2 112.0)
      (111.0 115.5 119.5 102.1 102.0)))
  (defparameter *test-arr-data-01*
    (make-array '(5 5) :initial-contents *test-data-01*)))

(defparameter *asd-01* (make-approximation-array-bilinear
			*test-arr-data-01* 
			(vector  2.0  1.0 0.0 -1.0 -2.0)
			(vector -2.0 -1.0  0.0 1.0 2.0)))

(approximation-bilinear 2.5 -0.001
	(vector 2.0 1.0 0.0 -1.0 -2.0)
	(vector -2.0 -1.0 0.0 1.0 2.0)
	*asd-01*)

(loop :for h :from -25 :to 25 :collect
     (loop :for x :from -25 :to 25 :collect
	  (approximation-bilinear (/ h 10.0) (/ x 10.0)
				  (vector 2.0 1.0 0.0 -1.0 -2.0)
				  (vector -2.0 -1.0 0.0 1.0 2.0)
				  *asd-01*)))

(defun gnuplot-2dlist (2dlist x1-lst x2-lst))
