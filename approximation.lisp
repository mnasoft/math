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

(progn
  (defparameter *test-data-01*
    '((117.0 120.1 118.6 112.5 115.0)
      (115.0 125.2 119.7 122.4 112.0)
      (113.0 135.3 129.8 132.3 117.0)
      (112.0 122.4 124.9 122.2 112.0)
      (111.0 115.5 119.5 102.1 102.0)))
  (defparameter *test-arr-data-01*
    (make-array '(5 5) :initial-contents *test-data-01*)))

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

(defun make-appoximatio-array-bilinear (a2d)
  "Генерирует массив, содержащий в своих элементах функции двух переменных.
Функции представляют билинейную интерполяцию массива a2d.
Результирующий массив имеет размерности на единицу меньшую по всем направлениям (строкам и столбцам)."
  (let ((a2-rez (make-array (mapcar #'1- (array-dimensions a2d)) :initial-element nil)))
    (loop :for r :from 0 :below (array-dimension a2-rez 0) :do
	 (loop :for c :from 0 :below (array-dimension a2-rez 1) :do
	      (setf (aref a2-rez r c)
		    (make-belinear-interpolation
		     (list
		      (list -1.0 -1.0 (aref a2d r c))
		      (list -1.0  1.0 (aref a2d r (1+ c)))
		      (list  1.0 -1.0  (aref a2d (1+ r) c))
		      (list  1.0  1.0  (aref a2d (1+ r) (1+ c))))
		     :ff *apr-func-4*))))
    a2-rez))


(defun test-make-appoximatio-array-bilinear (r c func-arr)
  (make-array '(2 2)
	      :initial-contents
	      (list
	       (list (funcall (aref func-arr r c) -1 -1)
		     (funcall (aref func-arr r c) -1  1))
	       (list (funcall (aref func-arr r c)  1 -1)
		     (funcall (aref func-arr r c)  1  1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Тестирование
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *asd* (make-appoximatio-array-bilinear *test-arr-data-01*))

(defun foo-cols (x x-num-func)
  (let ((col (round (+ (- x  1/2)(/ x-num-func 2)))))
    (cond
      ((< col 0) 0)
      ((>= col x-num-func) 3)
      (t col))))

(defun foo-x (x x-num-func)
  (let* ((col (+ (- x  1/2)(/ x-num-func 2))))
    (* 2 (- col (foo-cols x x-num-func) ))))

(defun foo-rows (y y-num-func)
  (let ((row (round (+ (- (* y -1)  1/2)(/ y-num-func 2)))))
    (cond
      ((< row 0) 0)
      ((>= row y-num-func) 3)
      (t row))))

(defun foo-y (y y-num-func)
  (let* ((row (+ (- (* y -1) 1/2) (/ y-num-func 2))))
    (* 2 (- row (foo-rows y y-num-func)))))

(defun foo (x y x-num-func y-num-func)
  (list
   (list (foo-x x x-num-func) (foo-y y y-num-func))
   (list (foo-rows y y-num-func)
	 (foo-cols x x-num-func))))

(defun bar (x y a2d-func )
  (let* ((x-num-func (array-dimension a2d-func 1))
	 (y-num-func (array-dimension a2d-func 0))
	 (n (foo x y x-num-func y-num-func))
	 (x1-x2 (first n))
	 (x1    (first  x1-x2))
 	 (x2    (second x1-x2))
	 (rc    (second n))
	 (r     (first  rc))
 	 (c     (second rc)))
    (values (funcall (aref a2d-func r c) x2 x1 ) "x=" x "y=" y "rc=" rc "x1-x2" x1-x2 (test-make-appoximatio-array-bilinear r c a2d-func) *test-arr-data-01*)))

(bar 1.0 0.0 *asd*)

(+ 1247.5 (* (- 1247.5 474.9 ) 0.165))
