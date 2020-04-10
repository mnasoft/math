;;;; matr-class.lisp

(in-package #:math)
(annot:enable-annot-syntax)

@annot.class:export-class
(defclass <matrix> ()
  ((data :accessor matrix-data :initform nil :initarg :data)))

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
(defun matr-new* (rows cols &optional (lst nil))
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

@export
(defmethod equivalent ((m1 <matrix>) (m2 <matrix>) &key (test #'equal))
  (let ((rez t))
    (if (and (= (rows m1) (rows m2))
	     (= (cols m1) (cols m2)))
	(loop :for r :from 0 :below (rows m1) :do
	   (loop :for c :from 0 :below (cols m1) :do
		(setf rez (and rez (funcall test (mref m1 r c ) (mref m2 r c) )))))
	(setf rez nil))
    rez))

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
  "Извлекает главную диагональ матрицы. Элементы возвращаются в порядке возрастания строк.
Пример использования:
 (defparameter *mm* (make-instance '<matrix> :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0) (7d0 8d0 9d0) (10d0 11d0 12d0))))
 =>
 Matr 4х3
 [ 1.0d0     2.0d0     3.0d0    ]
 [ 4.0d0     5.0d0     6.0d0    ]
 [ 7.0d0     8.0d0     9.0d0    ]
 [ 10.0d0    11.0d0    12.0d0   ]

 (main-diagonal *mm*)
 (1.0d0 5.0d0 9.0d0)
"
  (loop :for i :from 0 :below (min (rows mm) (cols mm))
     :collect (mref mm i i)))

(defmethod (setf main-diagonal) (elements (mm <matrix>) )
  "!!!Извлекает главную диагональ матрицы. Элементы возвращаются в порядке возрастания строк.
Пример использования:
 (defparameter *mm* (make-instance '<matrix> :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0) (7d0 8d0 9d0) (10d0 11d0 12d0))))
 =>
 Matr 4х3
 [ 1.0d0     2.0d0     3.0d0    ]
 [ 4.0d0     5.0d0     6.0d0    ]
 [ 7.0d0     8.0d0     9.0d0    ]
 [ 10.0d0    11.0d0    12.0d0   ]

 (main-diagonal *mm*)
 (1.0d0 5.0d0 9.0d0)
"
  (loop :for i :from 0 :below (min (rows mm) (cols mm))
     :for el :in elements :by #'cdr  :do
       (setf (mref  mm i i) el))
  mm)

@export
(defmethod  squarep ((mm <matrix>))
  (= (cols mm) (rows mm) ))

@export
(defmethod anti-diagonal ((mm <matrix>))
  "Извлекает побочную диагональ для квадратной матрицы
Пример использования:
 (defparameter *mm* (make-instance '<matrix> :initial-contents '((1d0 2d0 3d0) 
							       (4d0 5d0 6d0) 
							       (7d0 8d0 9d0))))
 =>
 Matr 3х3
 [ 1.0d0     2.0d0     3.0d0    ]
 [ 4.0d0     5.0d0     6.0d0    ]
 [ 7.0d0     8.0d0     9.0d0    ]

 (anti-diagonal  *mm*)
"
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
  "Формирует точки для расчета коэффициентов по методу наименьших квадратов
 vv     - '(xx yy) - список, состоящий из имен факторов влияния
         и имени функции отклика;
 ff     - '((xx xx) (xx) (1.0) (yy)) - задает 
         вид функциональной зависимости;
 ex_pts - '((-1.0 1.0) (2.0 4.0) (3.0 9.0))  - задает 
         значения факторов влияния и значение функции отклика
Пример использования:
  (matr-las-gauss  
   (matr-mnk '(xx yy) 
	     '((xx xx) (xx) (1.0) (yy)) 
	     '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0))))
 
 (\"Matr\" 1 3 ((0 . 1.0d0) (1 . 0.0d0) (2 . 0.0d0)))
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
(defmethod matr-triang* ((matr <matrix> ))
  "Выполняет приведение  матрицы  к треугольному виду, для решения системы ЛУ методом Гаусса;
Пример использования 1
 (matr-print 
 (matr-triang (matr-new 3 4 '(0.0 0.0 4.0 12.0
			      2.0 0.0 2.0 8.0
			      0.0 3.0 0.0 6.0))))
 (matr-triang* (make-instance '<matrix> :initial-contents '((0.0 0.0 4.0 12.0)
							  (2.0 0.0 2.0  8.0)
							  (0.0 3.0 0.0  6.0))))
 Matr 3х4
 [ 1.0       0.0       1.0       4.0      ]
 [ 0.0       1.0       0.0       2.0      ]
 [ 0.0       0.0       1.0       3.0      ]
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

 (matr-triang* (make-instance '<matrix> :initial-contents 
			      '((1.0d0  2.0d0  3.0d0  4.0d0)
				(5.0d0  6.0d0  7.0d0  8.0d0)
				(9.0d0 10.0d0 11.0d0 12.0d0))))
=>
Matr 3 4
 (1.0D0 2.0D0 3.0D0 4.0D0)
 (-0.0D0 1.0D0 2.0D0 2.9999999999999996D0)
 (0.0D0 0.0D0 0.0D0 8.881784197001252D-16)
"
  (do ((n (rows matr)) (ie nil) (j 0 (1+ j)) (row_j nil) (row_i nil))
      ((>= j n) matr)
    (setf ie (1- n))
    (do ((i j (1+ i)) (matr_ij nil) (row_ie nil)) ; Цикл перестановки строк в j-товом столбце которых присутстыуют нули
	((> i ie))
      (setf row_i   (row matr i)
	    matr_ij (mref matr i j))
      (cond ((= matr_ij 0) ; Перестановка i-товой строки в место поледней непереставленной
	     (setf row_ie (row matr ie) ; Последняя непереставленная строка
		   (row matr i) row_ie 
		   (row matr ie) row_i
		   ie (1- ie)) ; Увеличение количества переставленных строк
	     (decf i)) ; Уменьшение переменной цикла для выполнения повторной итерации
	    ((/= matr_ij 0)
	     (setf row_i (mapcar #'(lambda (el) (/ el matr_ij)) row_i) ; Деление строки на matr_ij элемент матрицы
		   (row matr i) row_i))))
    (setf row_j (row matr j)) ; Строка которую необходимо вычесть из других строк
    (do ((i (1+ j)(1+ i))) ; Цикл для вычитания j-товой строки из i-товой
	((> i ie))			
      (setf row_i (row matr i)
	    row_i (mapcar #'(lambda (el1 el2) (- el1 el2)) row_i row_j)
	    (row matr i) row_i)
;;;;(matr-print matr)
      )))

@export
(defmethod matr-obrhod* ((matr <matrix>))
  "Обратный ход при вычислении решения системы линейных уравнений;
Матрица matr должна быть приведена к треуголной;"
  (let* ((n (rows matr)) ; Количество строк в матрице (матрица расширенная)
	 (x (matr-new* 1 n))) ; Вектор результат
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
(defmethod matr-las-gauss* ((matr <matrix>))
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
 =>Matr 1 3
 (1 2 3)
"
  (let* ((matr-tr (matr-triang* matr))
	 (x (matr-obrhod* matr-tr)))
    x))

@export
(defun matr-osr-body* (vv ff ex_pts)
  "Пример использования:
  (matr-osr-body* '(xx yy) 
		 '((xx xx) (xx) (1.0) (yy)) 
		 '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0)))
;
=>(\"Matr\" 1 3 ((0 . 1.0d0) (1 . 0.0d0) (2 . 0.0d0)))
"
  (let ((kk
	 (cons '+
	     (mapcar
	      #'(lambda(el1 el2)
		  (cons '* (cons el1 el2)))
	      (row (matr-las-gauss* (make-least-squares-matrix vv ff ex_pts)) 0)
	      ff)))
	(rez nil))
    (setf rez (list (reverse (cdr(reverse vv))) kk))
    rez))

@export
(defun matr-osr-lambda* (vv ff ex_pts)
  "Пример использования:
  (matr-osr-lambda* '(xx yy) 
		 '((xx xx) (xx) (1.0) (yy)) 
		 '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0)))
;
=>(\"Matr\" 1 3 ((0 . 1.0d0) (1 . 0.0d0) (2 . 0.0d0)))
"
  (cons 'lambda (matr-osr-body* vv ff ex_pts)))

@export
(defun matr-osr-func* (vv ff ex_pts func-name)
  "Пример использования:

 (matr-osr-func* '(xx yy) 
		 '((xx xx) (xx) (1.0) (yy)) 
		 '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0))
		 'coool-func)

;
=>(\"Matr\" 1 3 ((0 . 1.0d0) (1 . 0.0d0) (2 . 0.0d0)))
"
  (cons 'defun (cons func-name (matr-osr-body* vv ff ex_pts))))


@export
(defmethod matr-sum* ((a <matrix> ) (b <matrix>))
  "Выполняет сложение матриц a и b.
Пример использования:
 (matr-sum* (matr-new* 2 2 '(1 2 3 4)) (matr-new* 2 2 '(1 2 3 4)))
 (matr-sum* (matr-new* 2 2 '(1 2 3 4)) (matr-new* 2 2 '(4 3 2 1)))
"
  (assert (and (= (rows a) (rows b)) (= (cols a) (cols b))) (a b)
	  "Матрицы A[~A,~A] и B[~A,~A] имеют отличаюшиеся размеры"
	  (rows a) (cols a) (rows b) (cols b))
  (let ((a+b (matr-new* (rows a) (cols a))))
    (loop :for r :from 0 :below (rows a) :do
	 (loop :for c :from 0 :below (rows b) :do
	      (setf (mref a+b r c)
		    (+ (mref a r c ) (mref b r c)))))
    a+b))

@export
(defmethod matr-mult* ((a <matrix> ) (b <matrix>))
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
  (let ((a_n (rows a))
	(a_m (cols a))
	(b_n (rows b))
	(b_m (cols b))
	(c nil))
    (assert (= a_m b_n) (a_m b_n) "Запрещенные размеры матриц для перемножения: A[~A,~A] x B[~A,~A]" a_n a_m b_n b_m)
    (setf c (make-instance '<matrix> :dimensions (list a_n b_m) :initial-element 0.0))
    
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
(defmethod matr-mult* ((a number ) (b <matrix>))
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
