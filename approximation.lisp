;;;; approximation.lisp

(annot:enable-annot-syntax)

(in-package #:math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@doc
"@b(Описание:) функция @b(appr-table) возвращает результат линейной 
интерполяции (или экстраполяции) для значения @b(x) на таблице @b(table).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (appr-table 0.5 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0))) => 0.5
 (appr-table 1.5 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0))) => 2.5
 (appr-table 3.0 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0))) => 2.0
 Тест: (test-approximation-appr-table)
@end(code)
"
(defun appr-table (x table)
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
;;;; Аппроксимационные полиномиальные зависимости функции нескольких переменных.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@doc
" @b(Пример использования:)
@begin[lang=lisp](code)
 (averaging-function-body '(xx yy) 
		 '((xx xx) (xx) (1.0) (yy)) 
		 '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0)))

 => ((XX) (+ (* 1.0d0 XX XX) (* 0.0d0 XX) (* 0.0d0 1.0)))
@end(code)
"
(defun averaging-function-body (vv ff ex_pts)
  (let ((kk
	 (cons '+
	     (mapcar
	      #'(lambda(el1 el2)
		  (cons '* (cons el1 el2)))
	      (row (solve-linear-system-gauss (make-least-squares-matrix vv ff ex_pts)) 0)
	      ff)))
	(rez nil))
    (setf rez (list (reverse (cdr(reverse vv))) kk))
    rez))

@export
@doc
"
 @b(Пример использования:)
@begin[lang=lisp](code)
 (averaging-function-lambda '(xx yy) 
		 '((xx xx) (xx) (1.0) (yy)) 
		 '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0)))
 => (LAMBDA (XX) (+ (* 1.0d0 XX XX) (* 0.0d0 XX) (* 0.0d0 1.0)))
@end(code)"
(defun averaging-function-lambda (vv ff ex_pts)
  (cons 'lambda (averaging-function-body vv ff ex_pts)))

@export
@doc
"
 @b(Пример использования:)
@begin[lang=lisp](code)
 (averaging-function-defun '(xx yy) 
			   '((xx xx) (xx) (1.0) (yy)) 
			   '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0))
			   'coool-func)
 => (DEFUN COOOL-FUNC (XX) (+ (* 1.0d0 XX XX) (* 0.0d0 XX) (* 0.0d0 1.0)))
@end(code)
"
(defun averaging-function-defun (vv ff ex_pts func-name)
  (cons 'defun (cons func-name (averaging-function-body vv ff ex_pts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Разработка линейной интерполяции функции одного переменного
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@doc
"@b(Описание:) функция @b(make-linear-interpolation) возвращает функциональной 
зависимость, аппроксимируемую по функции одного переменного, заданной таблично.

 @b(Переменые:)
@begin(list)
@item(points - список, состоящий из аргумента и значения;)
@item(ff - вид функциональной зависимости см. *apr-func-1-2* --- *apr-func-1-5*.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let* ((points-01 '((0.0 0.0) (1.0 1.0) (2.0 2.0)))
	(func-1 (make-linear-interpolation points-01)))
   (loop :for x :from 0 :to 2 :by 1/10
	 :collect (mapcar #'(lambda (el) (coerce el 'single-float))
			  (list x
				(funcall func-1 x)))))
 =>  ((0.0 0.0) (0.1 0.1) (0.2 0.2) (0.3 0.3) (0.4 0.4) (0.5 0.5) (0.6 0.6)
      (0.7 0.7) (0.8 0.8) (0.9 0.9) (1.0 1.0) (1.1 1.1) (1.2 1.2) (1.3 1.3)
      (1.4 1.4) (1.5 1.5) (1.6 1.6) (1.7 1.7) (1.8 1.8) (1.9 1.9) (2.0 2.0))

 (let* ((points-02 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (3.0 9.0)))
	(func-2 (make-linear-interpolation points-02 :ff *apr-func-1-3*)))
   (loop :for x :from 1 :to 3 :by 1/10
	 :collect (mapcar #'(lambda (el) (coerce el 'single-float))
			  (list x
				(funcall func-2 x)))))
  =>  ((1.0 1.0) (1.1 1.21) (1.2 1.44) (1.3 1.69) (1.4 1.96) (1.5 2.25) (1.6 2.56)
       (1.7 2.89) (1.8 3.24) (1.9 3.61) (2.0 4.0) (2.1 4.41) (2.2 4.84) (2.3 5.29)
       (2.4 5.76) (2.5 6.25) (2.6 6.76) (2.7 7.29) (2.8 7.84) (2.9 8.41) (3.0 9.0))
@end(code)
"
(defun make-linear-interpolation (points &key (ff *apr-func-1-2*))
  (eval (averaging-function-lambda *apr-args-1* ff points)))

@doc
"STUB"
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

@doc
"@b(Описание:) функция @b(index-by-value)
 (index-by-value 10 (vector 1 2 3 4 5))
"
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

@doc
"@b(Описание:) функция @b(approximation-linear) 
"
(defun approximation-linear (x1 a-x1 a-func)
  (funcall (aref a-func (index-by-value x1 a-x1)) x1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export-class
@doc
"@b(Описание:) Класс @b(<appr-linear>) представляет линейную интерполяцию.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((a1 (make-instance '<appr-linear>
			  :x1  (vector 1 2 3)
			  :a1d (vector 1 4 9)))
       (a2 (make-instance '<appr-linear>
 			  :x1  (vector 2 3  4)
 			  :a1d (vector 4 9 16))))
   (loop :for i :from 0 :to 5 :by 1/5 :do
     (format t \"| ~5F | ~5F | ~5F |~%\" i (approximate i a1) (approximate i a2))))
 =>
    | 0.0 |  -2.0 |  -6.0 |
    | 0.2 |  -1.4 |  -5.0 |
    | 0.4 |  -0.8 |  -4.0 |
    | 0.6 |  -0.2 |  -3.0 |
    | 0.8 |   0.4 |  -2.0 |
    | 1.0 |   1.0 |  -1.0 |
    | 1.2 |   1.6 |  -0.0 |
    | 1.4 |   2.2 |   1.0 |
    | 1.6 |   2.8 |   2.0 |
    | 1.8 |   3.4 |   3.0 |
    | 2.0 |   4.0 |   4.0 |
    | 2.2 |   5.0 |   5.0 |
    | 2.4 |   6.0 |   6.0 |
    | 2.6 |   7.0 |   7.0 |
    | 2.8 |   8.0 |   8.0 |
    | 3.0 |   9.0 |   9.0 |
    | 3.2 |  10.0 |  10.4 |
    | 3.4 |  11.0 |  11.8 |
    | 3.6 |  12.0 |  13.2 |
    | 3.8 |  13.0 |  14.6 |
    | 4.0 |  14.0 |  16.0 |
    | 4.2 |  15.0 |  17.4 |
    | 4.4 |  16.0 |  18.8 |
    | 4.6 |  17.0 |  20.2 |
    | 4.8 |  18.0 |  21.6 |
    | 5.0 |  19.0 |  23.0 |
@end(code)
"
(defclass <appr-linear> ()
  ((x1       :accessor appr-linear-x1       :documentation "Вектор аргументов.")
   (a1d-func :accessor appr-linear-a1d-func :documentation "Вектор функций.")))

(defmethod print-object ((a-l <appr-linear>) s)
  (format s "#<appr-linear> (~A ~A)"
	  (appr-linear-x1       a-l)
	  (appr-linear-a1d-func a-l)))

(defmethod initialize-instance ((a-l <appr-linear>) &key (x1 (vector -2 -1 0 1 2)) (a1d (vector 4 1 0 1 4)))
  (setf (appr-linear-x1       a-l) x1
	(appr-linear-a1d-func a-l) (make-linear-approximation-array x1 a1d)))

@export @doc "@b(Описание:) метод @b(approximate) возвращает значение функции одного переменного 
в точке point для функции заданой таблично и аппроксимированной объектом @b(a-l).
"
(defmethod approximate ((point number) (a-l <appr-linear>))
  (approximation-linear point (appr-linear-x1 a-l) (appr-linear-a1d-func a-l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Линейная интерполяции функции двух переменных (билинейная интерполяция)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-belinear-interpolation (points &key (ff *apr-func-2-4*))
  "Создает объект билинейной интерполяции
Пример использования:
  (make-belinear-interpolation
       '((0.0 0.0 10.0)
	 (0.0 1.0 2.0)
	 (1.0 0.0 5.0)
	 (1.0 1.0 15.0)))
"
  (eval (averaging-function-lambda '(x1 x2 yy) ff points)))

@doc
"@b(Описание:) функция @b(make-bilinear-approximation-array) генерирует 
массив, содержащий в своих элементах функции двух переменных.
Функции представляют билинейную интерполяцию массива a2d.
Результирующий массив имеет размерности на единицу меньшую по всем направлениям (строкам и столбцам).
@begin[lang=lisp](code)
|-------+---+----------+----------+----------+----------+----------|
|       |   | x2[0]    | x2[1]    | x2[2]    | x2[3]    | x2[4]    |
|-------+---+----------+----------+----------+----------+----------|
|-------+---+----------+----------+----------+----------+----------|
| x1[0] |   | a2d[0,0] | a2d[0,1] | a2d[0,2] | a2d[0,3] | a2d[0,4] |
| x1[1] |   | a2d[1,0] | a2d[1,1] | a2d[1,2] | a2d[1,3] | a2d[1,4] |
| x1[2] |   | a2d[2,0] | a2d[2,1] | a2d[2,2] | a2d[2,3] | a2d[2,4] |
| x1[3] |   | a2d[3,0] | a2d[3,1] | a2d[3,2] | a2d[3,3] | a2d[3,4] |
| x1[4] |   | a2d[4,0] | a2d[4,1] | a2d[4,2] | a2d[4,3] | a2d[4,4] |
@end(code)
"
(defun make-bilinear-approximation-array (a2d x1 x2)
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

@doc
"STUB"
(defun approximation-bilinear (v1 v2 a-x1 a-x2 a-func)
  (funcall (aref a-func (index-by-value v1 a-x1) (index-by-value v2 a-x2)) v1 v2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export-class
@doc
"@b(Описание:) Класс @b(<appr-bilinear>) представляет билинейную интерполяцию."
(defclass <appr-bilinear> ()
  ((x1       :accessor appr-bilinear-x1 :documentation "Вектор реперных значений по первому направлению (измерению).")
   (x2       :accessor appr-bilinear-x2 :documentation "Вектор реперных значений по второму направлению (измерению).")
   (a2d-func :accessor appr-bilinear-a2d-func :documentation "Двумерный массив функций размерности которого, на единицу 
меньше количества реперных значений по соответствующему направлению (измерению).")))

(defmethod print-object ((a-l <appr-bilinear>) s)
  (format s "#<appr-bilinear> (~A ~A ~A)"
	  (appr-bilinear-x1       a-l)
 	  (appr-bilinear-x2       a-l)
	  (appr-bilinear-a2d-func a-l)))

(defmethod initialize-instance ((a-l <appr-bilinear>) &key (x1 (vector -1 0 1 )) (x2 (vector 0 1 )) (a2d '#2A((1 2) (3 4) (5 6))))
  (setf (appr-bilinear-x1       a-l) x1
	(appr-bilinear-x2       a-l) x2
	(appr-bilinear-a2d-func a-l) (make-bilinear-approximation-array  a2d x1 x2)))

@export
@doc
"@b(Описание:) метод @b(approximate)
"
(defmethod approximate (point (a-l <appr-bilinear>))
  (approximation-bilinear (aref point 0) (aref point 1) (appr-bilinear-x1 a-l) (appr-bilinear-x2 a-l) (appr-bilinear-a2d-func a-l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Сглаживание методами gnuplot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export (defgeneric approx-by-points (pnt d-pnt points values &key w-func)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@doc
"Выполняет поиск массива значений такого, что:
- при сглаживании функцией w-func;
- с размером сглаживания dx0;
- в точках points (аргументы функции);
- сумма отклонений сглаженных значений от значений, заданных в узлах не превысит значения delta."
(defgeneric refine-approximation-values (points values base-dist-s &key w-func delta iterations))

(defmethod refine-approximation-values ((points array) (values vector) (base-dists vector) &key (w-func #'gauss-smoothing) (delta 0.001) (iterations 10000))
  "Вычисляет такой массив, что при сглаживании его по формуле Гаусса
с характерным размером base-dists, сумма расстояний до 2d точек заданных массивом points не превысит delta

  Пример использования:

 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 1.0 1.0))
 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 0.6 0.6))
 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 0.4 0.4))

 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 1.0 1.0)) 
 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 0.6 0.6)) 
 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 0.4 0.4)) 

 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 1.0 1.0)) 
 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 0.6 0.6)) 
 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 0.4 0.4)) 

 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 1.0 1.0)) 
 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 0.6 0.6)) 
 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 0.4 0.4)) 

 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 1.0 1.0)) 
 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 0.6 0.6)) 
 (refine-approximation-values (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) (vector 0.0 1.0 1.0 2.0) (vector 0.4 0.4)) 
"
  (assert (member w-func (list #'gauss-smoothing #'exp-smoothing #'cauchy-smoothing #'hann-smoothing)))
  (assert (= (array-rank points) 2))
  (assert (= (array-dimension points 0) (length values)))
  (assert (= (array-dimension points 1) (length base-dists)))
  (let ((v-iter (copy-array values))
	(v-rez  (copy-array values)))
    (labels ((start-V1 (v-rez v-iter)
	       (loop :for i :from 0 :below (length values) :do
		    (setf (svref v-rez i) (approx-by-points (row i points) base-dists points v-iter :w-func w-func)))
	       (summ-distance v-rez values))
	     (iterate (v-rez v-iter)
	       (loop :for i :from 0 :below (length values) :do
		    (setf (svref v-iter i)
			  (+ (svref v-iter i) (* 1 (- (svref values i) (svref v-rez i))))))))
      (do ((i    0 (1+ i))
	   (dist (start-V1 v-rez v-iter ) (start-V1 v-rez v-iter)))
	  ((or (> i iterations) (< dist delta))
	   (progn (format t "I=~D; DIST=~F;~%V-ITER~S;~%V-REZ=~S~%" i dist v-iter v-rez )
		  (if (< i iterations)
		      (values v-iter t   i dist v-rez values)
		      (values v-iter nil i dist v-rez values))))
	(iterate v-rez v-iter)))))

(defmethod refine-approximation-values ((points vector) (values vector) (base-dist number) &key (w-func #'gauss-smoothing) (delta 0.001) (iterations 10000))
  (assert (member w-func (list #'gauss-smoothing #'exp-smoothing #'cauchy-smoothing #'hann-smoothing)))
  (assert (= (length points) (length values)))
  (let ((v-iter (copy-array values))
	(v-rez  (copy-array values)))
    (labels ((start-V1 (v-rez v-iter)
	       (loop :for i :from 0 :below (length values) :do
		    (setf (svref v-rez i)
			  (approx-by-points (svref points i) base-dist points v-iter :w-func w-func)))
	       (summ-distance v-rez values))
	     (iterate (v-rez v-iter)
	       (loop :for i :from 0 :below (length values) :do
		    (setf (svref v-iter i)
			  (+ (svref v-iter i) (* 1 (- (svref values i) (svref v-rez i))))))))
      (do ((i    0 (1+ i))
	   (dist (start-V1 v-rez v-iter ) (start-V1 v-rez v-iter)))
	  ((or (> i iterations) (< dist delta))
	   (progn (format t "I=~D; DIST=~F;~%V-ITER~S;~%V-REZ=~S~%" i dist v-iter v-rez )
		  (if (< i iterations)
		      (values v-iter t   i dist v-rez values)
		      (values v-iter nil i dist v-rez values))))
	(iterate v-rez v-iter)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



