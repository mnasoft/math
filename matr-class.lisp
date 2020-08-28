;;;; matr-class.lisp

(in-package #:math)

(annot:enable-annot-syntax) 

@export-class
(defclass <matrix> ()
  ((data :accessor matrix-data :initform nil :initarg :data
	 :documentation "Сдержимое матрицы."))
  (:documentation "Представляет матрицу, определенную через массив.

Создание:
@begin(list)
 @item(при помощи функции matr-new)
 @item( )
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (matr-new 2 3)
 => Matr 2х3
    [ 0.0d0     0.0d0     0.0d0    ]
    [ 0.0d0     0.0d0     0.0d0    ]
 (matr-new 3 2 '(1 2 3 4 5))
 => Matr 3х2
    [ 1         2        ]
    [ 3         4        ]
    [ 5         NIL      ]
@end(code)

"))

@doc
"@b(Описание:) функция|метод|обобщенная_функция| @b(...)
"
(defmethod matr-name-* ((mm <matrix>)) "Matr")

(defmethod print-object ((mm <matrix>) s)
  (format s "~A " (matr-name-* mm))
  (when (and (matrix-data mm) (arrayp (matrix-data mm)))
    (format s "~{~A~^х~}" (array-dimensions (matrix-data mm)))
    (loop :for i :from 0 :below (array-dimension (matrix-data mm) 0)
       :do
	 (format s "~%[")
	 (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1)
	    :do (format s " ~8A " (aref (matrix-data mm) i j)))
	 (format s "]"))))

@export
(defmethod initialize-instance ((mm <matrix>) &key dimensions initial-element initial-contents data (element-type t))
  (when (and (consp dimensions) (/= (length dimensions) 2))
    (error "(/= (length dimensions) 2):"))
  (cond
    (data (setf (matrix-data mm) data))
    ((and (consp dimensions) (= (length dimensions) 2) initial-element)
     (setf (matrix-data mm)
	   (make-array dimensions
		       :element-type element-type :initial-element initial-element)))
    ((and (consp dimensions) (= (length dimensions) 2) (null initial-element )
	  (setf (matrix-data mm)
		(make-array dimensions
			    :element-type element-type :initial-element 0))))
    ((and (consp initial-contents) (consp (first initial-contents)))
     (setf (matrix-data mm)
	   (make-array (list (length initial-contents) (length (first initial-contents)))
		       :element-type element-type :initial-contents initial-contents)))
    (t (error "(defmethod initialize-instance ((mm <matrix>) &key dimensions initial-element initial-contents (element-type t))
Что-то пошло не так!"))))

@export
(defun matr-new (rows cols &optional (lst nil))
  "Примечание:
 (matr-new 3 4 '(1 2 3 4 5 6 7 8 9 10)) "
  (let ((mm (make-instance '<matrix> :dimensions (list rows cols) :initial-element 0.0d0))
	(ll lst))
    (when (consp lst)
	(loop :for i :from 0 :below (array-dimension (matrix-data mm) 0) :do
	     (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1) :do
		  (setf (aref (matrix-data mm) i j) (car ll)
			ll (cdr ll)))))
    mm))

@export
(defmethod mref ((mm <matrix>) i j) (aref (matrix-data mm) i j))

(defmethod (setf mref) (value (mm <matrix>) i j) (setf (aref (matrix-data mm) i j) value) mm)

@export
(defmethod copy ((mm-ref <matrix>))
  (make-instance '<matrix> :data (copy-array (matrix-data mm-ref))))

@export
(defmethod dimensions ((mm <matrix>)) (array-dimensions (matrix-data mm)))

@export
(defmethod rows ((mm <matrix>)) (array-dimension (matrix-data mm) 0))

@export
(defmethod cols ((mm <matrix>)) (array-dimension (matrix-data mm) 1))


;;;;;;;;;; equivalent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defun semi-equal (x y &key (tolerance (+ (* 0.000001 (sqrt (+ (* y y) (* x x)))) 0.000001)))
  (labels ((distance (x y) (abs (- x y))))
    (< (distance x y) tolerance)))

@export
(defmethod equivalent ((m1 <matrix>) (m2 <matrix>) &key (test #'semi-equal))
  (let ((rez t))
    (if (and (= (rows m1) (rows m2))
	     (= (cols m1) (cols m2)))
	(loop :for r :from 0 :below (rows m1) :do
	   (loop :for c :from 0 :below (cols m1) :do
		(setf rez (and rez (funcall test (mref m1 r c ) (mref m2 r c) )))))
	(setf rez nil))
    rez))

@export
(defmethod equivalent ((a1 array) (a2 array) &key (test #'semi-equal))
   (declare (type (array * (* *)) a1 a2))
   (when (not (equal (array-dimensions a1)
		     (array-dimensions a2)))
     (return-from math:equivalent nil))
   (reduce #'(lambda (el1 el2) (and el1 el2))
	   (apply #'append
		  (loop :for i :from 0 :below (array-dimension a1 0)
			:collect
			(loop :for j :from 0 :below (array-dimension a1 1)
			      :collect
			      (funcall test (aref a1 i j) (aref a2 i j)))))
	   :initial-value t))
@export
(defmethod equivalent ((m1 <matrix>) (a2 array) &key (test #'semi-equal))
  (declare (type (array * (* *)) a2))  
  (equivalent m1 (make-instance '<matrix> :data a2) :test test))

@export
(defmethod equivalent ((a1 array) (m2 <matrix>) &key (test #'semi-equal))
  (declare (type (array * (* *)) a1))
  (equivalent (make-instance '<matrix> :data a1) m2 :test test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defmethod row ((mm <matrix>) row)
  (let ((data (matrix-data mm)))
    (loop :for c :from 0 :below (cols mm)
       :collect (aref data row c))))

(defmethod (setf row) (new-value-lst (mm <matrix>) row )
  (let ((data (matrix-data mm))
	(ll new-value-lst))
    (loop :for c :from 0 :below (cols mm)
       :do (setf (aref data row c) (car ll)
		 ll (cdr ll)))
    mm))

@export
(defmethod col ((mm <matrix>) col)
  (let ((data (matrix-data mm)))
    (loop :for r :from 0 :below (rows mm)
       :collect (aref data r col))))

(defmethod (setf col) (new-value-lst (mm <matrix>) col )
  (let ((data (matrix-data mm))
	(ll new-value-lst))
    (loop :for r :from 0 :below (rows mm)
       :do (setf (aref data r col) (car ll)
		 ll (cdr ll)))
    mm))

@export
(defmethod main-diagonal ((mm <matrix>))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (defparameter *mm* 
   (make-instance '<matrix> :initial-contents 
    '(( 1d0  2d0  3d0) 
      ( 4d0  5d0  6d0) 
      ( 7d0  8d0  9d0) 
      (10d0 11d0 12d0))))
 *mm*  =>  Matr 4х3
           [ 1.0d0     2.0d0     3.0d0    ]
           [ 4.0d0     5.0d0     6.0d0    ]
           [ 7.0d0     8.0d0     9.0d0    ]
           [ 10.0d0    11.0d0    12.0d0   ]

(main-diagonal *mm*) =>  (1.0d0 5.0d0 9.0d0)
@end(code)"
  (loop :for i :from 0 :below (min (rows mm) (cols mm))
	:collect (mref mm i i)))

@export
@doc
"@b(Пример использования:)
@begin[lang=lisp](code)
 (defparameter *mm* 
   (make-instance '<matrix> 
      :initial-contents '(( 1d0  2d0  3d0) 
                          ( 4d0  5d0  6d0) 
                          ( 7d0  8d0  9d0) 
                          (10d0 11d0 12d0))))
 *mm* =>  Matr 4х3
          [ 1.0d0     2.0d0     3.0d0    ]
          [ 4.0d0     5.0d0     6.0d0    ]
          [ 7.0d0     8.0d0     9.0d0    ]
          [ 10.0d0    11.0d0    12.0d0   ]

 (setf (main-diagonal *mm*) '(11d0 22d0 33d0)) 
 *mm* => Matr 4х3
        [ 11.0d0    2.0d0     3.0d0    ]
        [ 4.0d0     22.0d0    6.0d0    ]
        [ 7.0d0     8.0d0     33.0d0   ]
        [ 10.0d0    11.0d0    12.0d0   ]
@end(code)"  
(defmethod (setf main-diagonal) (elements (mm <matrix>) )

  (loop :for i :from 0 :below (min (rows mm) (cols mm))
	:for el :in elements :by #'cdr  :do
	  (setf (mref  mm i i) el))
  mm)

@export
@doc
"@b(Пример использования:)
@begin[lang=lisp](code)
 (defparameter *mm* 
   (make-instance '<matrix> 
      :initial-contents '(( 1d0  2d0  3d0) 
                          ( 4d0  5d0  6d0) 
                          ( 7d0  8d0  9d0) 
                          (10d0 11d0 12d0))))
 *mm* =>  Matr 4х3
          [ 1.0d0     2.0d0     3.0d0    ]
          [ 4.0d0     5.0d0     6.0d0    ]
          [ 7.0d0     8.0d0     9.0d0    ]
          [ 10.0d0    11.0d0    12.0d0   ]

 (setf (main-diagonal *mm*) 11d0) 
  Matr 4х3
  [ 11.0d0     2.0d0    3.0d0   ]
  [ 4.0d0     11.0d0    6.0d0   ]
  [ 7.0d0      8.0d0   11.0d0   ]
  [ 10.0d0    11.0d0   12.0d0   ]

 *mm* => Matr 4х3
  [ 11.0d0    2.0d0     3.0d0    ]
  [ 4.0d0     11.0d0    6.0d0    ]
  [ 7.0d0     8.0d0     11.0d0   ]
  [ 10.0d0    11.0d0    12.0d0   ]
@end(code)"  
(defmethod (setf main-diagonal) ((element number) (mm <matrix>) )
  (loop :for i :from 0 :below (min (rows mm) (cols mm))
	:do (setf (mref  mm i i) element))
  mm)

@export
@doc
"@b(Пример использования:)
@begin[lang=lisp](code)
  (defparameter *mm* (make-instance '<matrix>   :dimensions '(2 3)))
  *mm* => Matr 2х3
          [ 0         0         0        ]
          [ 0         0         0        ]
  (squarep *mm*) => nil
  (defparameter *mm* (make-instance '<matrix>   :dimensions '(3 3)))
  (squarep *mm*)
@end(code)
"
(defmethod  squarep ((mm <matrix>))

  (= (cols mm) (rows mm) ))

@export
@doc
"@b(Пример использования:)
@begin[lang=lisp](code)
 (defparameter *mm* 
  (make-instance '<matrix> 
   :initial-contents '((1d0 2d0 3d0) 
                       (4d0 5d0 6d0) 
                       (7d0 8d0 9d0))))
 =>
 Matr 3х3
 [ 1.0d0     2.0d0     3.0d0    ]
 [ 4.0d0     5.0d0     6.0d0    ]
 [ 7.0d0     8.0d0     9.0d0    ]

  (anti-diagonal  *mm*) => (3.0d0 5.0d0 7.0d0)
  @end(code)"
(defmethod anti-diagonal ((mm <matrix>))
  (assert (squarep mm) (mm) "Матрица не является квадратной~%~S" mm)
  (loop
     :for c :from 0 :below (cols mm)
     :for r :downfrom (- (rows mm) 1) :to 0
     :collect (mref mm c r)))

(defmethod (setf anti-diagonal) (elements (mm <matrix>) )
  (assert (squarep mm) (mm) "Матрица не является квадратной~%~S" mm)
  (loop
     :for c :from 0 :below (cols mm)
     :for r :downfrom (- (rows mm) 1) :to 0
     :for e :in elements :by #'cdr :do
       (setf (mref mm c r) e))
  mm)

@export
(defun make-least-squares-matrix (vv ff ex_pts)
  "@b(Описание:) функция @b(make-least-squares-matrix) возвращает матрицу
для расчета коэффициентов полиномиальной зависимости вида @b(ff) со 
списком имен факторов влияния и имени функции отклика @b(vv),
которая приближается к экспериментальным точкам @b(ex_pts),
методом наименьших квадратов.

 @b(Переменые:)
@begin(list)
 @item(vv - список, состоящий из имен факторов влияния и имени функции отклика;)
 @item(ff - задает вид функциональной зависимости;)
 @item(ex_pts - задает значения факторов влияния и значение функции отклика;)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
;;;; Для аппроксимации экспериментальных точек полиномом второй степени функции одной переменной.
;;;; Здесь коэффициенты a1, a2, a3 можно найти решив САУ, получаемую в результате. 
;;;; (defun yy (xx) (+ (* a1 xx xx) (* a2 xx) a3))

 (make-least-squares-matrix 
  '(xx yy) 
  '((xx xx) (xx) (1.0) (yy)) 
  '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0)))
  => Matr 3х4
     [ 98.0d0    34.0d0    14.0d0    98.0d0   ]
     [ 34.0d0    14.0d0    4.0d0     34.0d0   ]
     [ 14.0d0    4.0d0     4.0d0     14.0d0   ]
;;;; Для аппроксимации экспериментальных точек полиномом второй степени функции двух переменных.
;;;; Здесь коэффициенты a1, a2, a3 можно найти решив САУ, получаемую в результате. 
     (defun yy (x1 x2) (+ (* 1.0 x1 x1) (* 2.0 x2 x2)  (* 3.0 x1 x2)  (* 4.0 x1) (* 5.0 x2) 6.0))
    
 (make-least-squares-matrix 
  '(x1 x2 yy) 
  '((x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy)) 
  (let ((pts nil))
    (loop :for i :from -2 :to 2 :do
      (loop :for j :from -2 to 2 :do
	(push (list i j (yy i j)) pts)))
    pts))

;  => Matr 6х7
; [ 170.0d0   100.0d0   0.0d0     0.0d0     0.0d0     50.0d0    670.0d0  ]
; [ 100.0d0   170.0d0   0.0d0     0.0d0     0.0d0     50.0d0    740.0d0  ]
; [ 0.0d0     0.0d0     100.0d0   0.0d0     0.0d0     0.0d0     300.0d0  ]
; [ 0.0d0     0.0d0     0.0d0     50.0d0    0.0d0     0.0d0     200.0d0  ]
; [ 0.0d0     0.0d0     0.0d0     0.0d0     50.0d0    0.0d0     250.0d0  ]
; [ 50.0d0    50.0d0    0.0d0     0.0d0     0.0d0     25.0d0    300.0d0  ]

  (solve-linear-system-gauss *)
;  => Matr 1х6
; [ 1.0d0     2.0d0     3.0d0     4.0d0     5.0d0     6.0d0    ]
@end(code)

 См. также solve-linear-system-gauss; solve-linear-system-rotation
  "
  (let* ((m          (length ff))
	 (n          (1- m))
	 (mtr        (make-instance '<matrix> :dimensions  (list n m) :initial-element 0.0d0 ))
	 (mtr-lambda (make-instance '<matrix> :dimensions  (list n m) :initial-element nil )))
    (dotimes (i n)
      (dotimes (j m)
	(setf (mref mtr-lambda i j)
	      (eval (list 'lambda  vv (cons '* (append (nth i ff) (nth j ff))))))))
    
    (mapc
     #'(lambda (el)
	 (dotimes (i n)
	   (dotimes (j m)
	     (setf (mref mtr i j)
		   (+ (apply (mref mtr-lambda i j) el) (mref mtr i j))))))
     ex_pts)
    mtr))

@export
@doc
"@b(Пример использования:)
@begin[lang=lisp](code)
 (convert-to-triangular 
  (make-instance '<matrix> 
		 :initial-contents '((0.0 0.0 4.0 12.0)
				     (2.0 0.0 2.0  8.0)
				     (0.0 3.0 0.0  6.0))))
 => Matr 3х4
    [ 1.0       0.0       1.0       4.0      ]
    [ 0.0       1.0       0.0       2.0      ]
    [ 0.0       0.0       1.0       3.0      ]
@end(code)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (convert-to-triangular
  (make-instance '<matrix> 
		 :initial-contents '((1.0d0  2.0d0  3.0d0  4.0d0)
				     (5.0d0  6.0d0  7.0d0  8.0d0)
				     (9.0d0 10.0d0 11.0d0 12.0d0))))
  => Matr 3х4
     [ 1.0d0     2.0d0     3.0d0     4.0d0    ]
     [ -0.0d0    1.0d0     2.0d0     2.9999999999999996d0 ]
     [ 0.0d0     0.0d0     0.0d0     8.881784197001252d-16 ]

@end(code)"
(defmethod convert-to-triangular ((matr <matrix> ))
  (do ((n (rows matr)) (ie nil) (j 0 (1+ j)) (row-j nil) (row-i nil))
      ((>= j n) matr)
    (setf ie (1- n))
    (do ((i j (1+ i)) (matr-ij nil) (row-ie nil)) ; Цикл перестановки строк в j-товом столбце которых присутстыуют нули
	((> i ie))
      (setf row-i   (row matr i)
	    matr-ij (mref matr i j))
      (cond ((= matr-ij 0) ; Перестановка i-товой строки в место поледней непереставленной
	     (setf row-ie (row matr ie) ; Последняя непереставленная строка
		   (row matr i) row-ie 
		   (row matr ie) row-i
		   ie (1- ie)) ; Увеличение количества переставленных строк
	     (decf i)) ; Уменьшение переменной цикла для выполнения повторной итерации
	    ((/= matr-ij 0)
	     (setf row-i (mapcar #'(lambda (el) (/ el matr-ij)) row-i) ; Деление строки на matr-ij элемент матрицы
		   (row matr i) row-i))))
    (setf row-j (row matr j)) ; Строка которую необходимо вычесть из других строк
    (do ((i (1+ j)(1+ i))) ; Цикл для вычитания j-товой строки из i-товой
	((> i ie))			
      (setf row-i (row matr i)
	    row-i (mapcar #'(lambda (el1 el2) (- el1 el2)) row-i row-j)
	    (row matr i) row-i))))

@export
@doc
"Обратный ход при вычислении решения системы линейных уравнений.

Матрица matr должна быть приведена к треугольной;"
(defmethod solve-linear-system-gauss-backward-run ((matr <matrix>))
  
  (let* ((n (rows matr)) ;; Количество строк в матрице (матрица расширенная)
	 (x (matr-new 1 n))) ;; Вектор результат
    (do ((i 0 (+ 1 i)))
	((>= i n) x)
      (setf (mref x 0 i) 1 ))
    (do ((i (- n 1) (- i 1)) (summ 0 0))
	((< i 0) x)
      (do ((j (+ 1 i) (+ 1 j)))
	  ((>= j  n) 'done2)
	(setf summ (+ summ (* (mref matr i j) (mref x 0 j)))))
      (setf (mref x 0 i) (/ (- (mref matr i n) summ) (mref matr i i))))))

@export
(defmethod solve-linear-system-gauss ((matr <matrix>))
  "@b(Пример использования 1.1:)
@begin[lang=lisp](code)
  (solve-linear-system-gauss
   (matr-new 3 4 '(1 2 3 14 
		   2 1 1 7 
		   3 0 1 2)))
 => Matr 1х3
    [ 1/3       16/3      1        ]
@end(code)

 @b(Пример использования 1.2:)
@begin[lang=lisp](code)
 (solve-linear-system-gauss
  (matr-new 3 4 '(1.0 2 3 14 
		  2 1 1 7 
		  3 0 1 2)))
 => Matr 1х3
    [ 0.33333397  5.333332  1.0000007 ]
@end(code)

 @b(Пример использования 2:)
@begin[lang=lisp](code)
 (solve-linear-system-gauss 
   (matr-new 3 4 '(1 0 1 4 
		   0 1 0 2 
		   0 0 1 3)))
  => Matr 1х3
     [ 1         2         3        ]
@end(code)
"
  (let* ((matr-tr (convert-to-triangular matr))
	 (x (solve-linear-system-gauss-backward-run matr-tr)))
    x))

@export
(defmethod add ((a <matrix> ) (b <matrix>))
  "Выполняет сложение матриц a и b.
Пример использования:
 (add (matr-new 2 2 '(1 2 3 4)) (matr-new 2 2 '(1 2 3 4)))
 (add (matr-new 2 2 '(1 2 3 4)) (matr-new 2 2 '(4 3 2 1)))
"
  (assert (and (= (rows a) (rows b)) (= (cols a) (cols b))) (a b)
	  "Матрицы A[~A,~A] и B[~A,~A] имеют отличаюшиеся размеры"
	  (rows a) (cols a) (rows b) (cols b))
  (let ((a+b (matr-new (rows a) (cols a))))
    (loop :for r :from 0 :below (rows a) :do
	 (loop :for c :from 0 :below (rows b) :do
	      (setf (mref a+b r c)
		    (+ (mref a r c ) (mref b r c)))))
    a+b))

@export
(defmethod multiply ((a <matrix> ) (b <matrix>))
  "@b(Описание:) метод @b(multiply (a <matrix> ) (b <matrix>))
вполняет операцию умножения матриц a и b.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (multiply (matr-new 2 3 '(1.0 2.0 3.0
			   4.0 5.0 6.0))
	   (matr-new 3 2 '(1.0 2.0
			   3.0 4.0
			   5.0 6.0)))
 => Matr 2х2
    [ 22.0 28.0 ]
    [ 49.0 64.0 ]
 (multiply (matr-new 3 2 '(1.0 2.0
			   3.0 4.0
			   5.0 6.0))
	   (matr-new 2 3 '(1.0 2.0 3.0
			   4.0 5.0 6.0)))
 => Matr 3х3
    [ 9.0  12.0 15.0 ]
    [ 19.0 26.0 33.0 ]
    [ 29.0 40.0 51.0 ]
@end(code)
"
  (let ((a_n (rows a))
	(a_m (cols a))
	(b_n (rows b))
	(b_m (cols b))
	(c nil))
    (assert (= a_m b_n) (a_m b_n) "Запрещенные размеры матриц для перемножения: A[~A,~A] x B[~A,~A]" a_n a_m b_n b_m)
    (setf c (make-instance '<matrix> :dimensions (list a_n b_m) :initial-element 0))
    
    (do ((i 0 (1+ i))
	 (a-row nil))
	((>= i a_n))
      (setf a-row (row a i))
      (do ((j 0 (1+ j))
	   (b-col nil))
	  ((>= j b_m))
	(setf b-col (col b j))
	(setf (mref c i j)
	      (apply #'+ (mapcar #'* a-row b-col)))))
    c))

@export
(defmethod multiply ((a number ) (b <matrix>))
  (let ((rez (make-instance '<matrix> :dimensions (dimensions b))))
    (loop :for i :from 0 :below (rows b) :do
	 (loop :for j :from 0 :below (cols b) :do
	      (setf (mref rez i j) (* a (mref b i j)))))
    rez))

@export
(defmethod matrix->2d-list ((mm <matrix>))
  (loop :for i :from 0 :below (rows mm) :collect
       (row mm i)))

@export
(defmethod transpose ((mm <matrix>))
  (let ((rez (make-instance '<matrix> :dimensions (nreverse(dimensions mm)))))
    (loop :for i :from 0 :below (rows mm) :do
	 (loop :for j :from 0 :below (cols mm) :do
	      (setf (mref rez j i) (mref mm i j))))
    rez))

(defmethod transpose ((mm cons))
  "Выполняет транспонирование"
  (apply #'mapcar #'list mm))

@export
(defmethod swap-rows*  ((mm <matrix>) i j)
  (assert (and (< -1 i (rows mm)) (< -1 j (rows mm))))
  (when (/= i j)
    (let ((row-i (row mm i))
	  (row-j (row mm j)))
      (setf (row mm i) row-j
	    (row mm j) row-i)))
  mm)

@export
(defmethod swap-cols*  ((mm <matrix>) i j)
  (assert (and (< -1 i (cols mm)) (< -1 j (cols mm))))
  (when (/= i j)
    (let ((col-i (col mm i))
	  (col-j (col mm j)))
      (setf (col mm i) col-j
	    (col mm j) col-i)))
  mm)

@export
(defmethod swap-rows  ((mm <matrix>) i j)
  (swap-rows* (copy mm) i j))

@export
(defmethod swap-cols  ((mm <matrix>) i j)
  (swap-cols* (copy mm) i j))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defmethod matr-eval-* ((mm <matrix>))
  "Мутная функция и непонятно как ее использовать и где?"
  (let ((rows (rows mm))
	(cols (cols mm))
	(mm-cp (copy  mm)))
    (loop :for i :from 0 :below rows
       :do
	 (loop :for j :from 0 :below cols
	    :do (setf (aref (matrix-data mm) i j) (eval (aref (matrix-data mm) i j)))))
    mm-cp))
