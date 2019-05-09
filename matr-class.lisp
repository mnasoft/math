;;;; matr-class.lisp

(in-package #:math)

(defclass matrix ()
  ((data :accessor matrix-data :initform nil :initarg :data)))

(defmethod matr-name-* ((mm matrix)) "Matr")

(defmethod print-object ((mm matrix) s)
  (format s "~A " (matr-name-* mm))
  (when (and (matrix-data mm) (arrayp (matrix-data mm)))
    (format s "~{~A~^х~}" (array-dimensions (matrix-data mm)))
    (loop :for i :from 0 :below (array-dimension (matrix-data mm) 0)
       :do
	 (format s "~%[")
	 (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1)
	    :do (format s " ~8A " (aref (matrix-data mm) i j)))
	 (format s "]"))))

(defmethod initialize-instance ((mm matrix) &key dimensions initial-element initial-contents data (element-type t))
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
    (t (error "(defmethod initialize-instance ((mm matrix) &key dimensions initial-element initial-contents (element-type t))
Что-то пошло не так!"))))

(defun matr-new* (rows cols &optional (lst nil))
  "Примечание:
 (matr-new 3 4 '(1 2 3 4 5 6 7 8 9 10)) "
  (let ((mm (make-instance 'matrix :dimensions (list rows cols) :initial-element 0.0d0))
	(ll lst))
    (when (consp lst)
	(loop :for i :from 0 :below (array-dimension (matrix-data mm) 0) :do
	     (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1) :do
		  (setf (aref (matrix-data mm) i j) (car ll)
			ll (cdr ll)))))
    mm))

(defmethod matr-ij-* ((mm matrix) i j) (aref (matrix-data mm) i j))

(defmethod matr-equal* ((m1 matrix) (m2 matrix) &key (test #'equal))
  (let ((rez t))
    (if (and (= (matr-rows-* m1) (matr-rows-* m2))
	     (= (matr-cols-* m1) (matr-cols-* m2)))
	(loop :for r :from 0 :below (matr-rows-* m1) :do
	   (loop :for c :from 0 :below (matr-cols-* m1) :do
		(setf rez (and rez (funcall test (matr-ij-* m1 r c ) (matr-ij-* m2 r c) )))))
	(setf rez nil))
    rez))

(defmethod matr-set-ij-* ((mm matrix) value row col) (setf (aref (matrix-data mm) row col) value) mm)

(defmethod matr-copy-* ((mm-ref matrix))
  (make-instance 'matrix :data (copy-array (matrix-data mm-ref))))

(defmethod matr-rows-*   ((mm matrix)) (array-dimension (matrix-data mm) 0))

(defmethod matr-cols-*   ((mm matrix)) (array-dimension (matrix-data mm) 1))

(defmethod matr-set-row-* ((mm matrix) row pts)
  (let ((data (matrix-data mm))
	(ll pts))
    (loop :for c :from 0 :below (matr-cols-* mm)
       :do (setf (aref data row c) (car ll)
		 ll (cdr ll)))
    mm))

(defmethod matr-get-row-* ((mm matrix) row)
  (let ((data (matrix-data mm)))
    (loop :for c :from 0 :below (matr-cols-* mm)
       :collect (aref data row c))))

(defmethod matr-set-col-* ((mm matrix) col pts)
  (let ((data (matrix-data mm))
	(ll pts))
    (loop :for r :from 0 :below (matr-rows-* mm)
       :do (setf (aref data r col) (car ll)
		 ll (cdr ll)))
    mm))

(defmethod matr-get-col-* ((mm matrix) col)
  (let ((data (matrix-data mm)))
    (loop :for r :from 0 :below (matr-rows-* mm)
       :collect (aref data r col))))

(defmethod main-diagonal ((mm matrix))
  "Извлекает главную диагональ матрицы. Элементы возвращаются в порядке возрастания строк.
Пример использования:
 (defparameter *mm* (make-instance 'matrix :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0) (7d0 8d0 9d0) (10d0 11d0 12d0))))
 =>
 Matr 4х3
 [ 1.0d0     2.0d0     3.0d0    ]
 [ 4.0d0     5.0d0     6.0d0    ]
 [ 7.0d0     8.0d0     9.0d0    ]
 [ 10.0d0    11.0d0    12.0d0   ]

 (main-diagonal *mm*)
 (1.0d0 5.0d0 9.0d0)
"
  (loop :for i :from 0 :below (min (matr-rows-* mm) (matr-cols-* mm))
     :collect (matr-ij-* mm i i)))

(defmethod  squarep ((mm matrix))
  (= (matr-cols-* mm) (matr-rows-* mm) ))

(defmethod main-diagonal-set ((mm matrix) elements)
  "Извлекает главную диагональ матрицы. Элементы возвращаются в порядке возрастания строк.
Пример использования:
 (defparameter *mm* (make-instance 'matrix :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0) (7d0 8d0 9d0) (10d0 11d0 12d0))))
 =>
 Matr 4х3
 [ 1.0d0     2.0d0     3.0d0    ]
 [ 4.0d0     5.0d0     6.0d0    ]
 [ 7.0d0     8.0d0     9.0d0    ]
 [ 10.0d0    11.0d0    12.0d0   ]

 (main-diagonal *mm*)
 (1.0d0 5.0d0 9.0d0)
"
  (loop :for i :from 0 :below (min (matr-rows-* mm) (matr-cols-* mm))
     :for el :in elements :by #'cdr  :do
       (matr-set-ij-* mm  el i i))
  mm)

 
(defmethod anti-diagonal ((mm matrix))
  "Извлекает побочную диагональ для квадратной матрицы
Пример использования:
 (defparameter *mm* (make-instance 'matrix :initial-contents '((1d0 2d0 3d0) 
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
     :for c :from 0 :below (matr-cols-* mm)
     :for r :downfrom (- (matr-rows-* mm) 1) :to 0
     :collect (matr-ij-* mm c r)))




(defun matr-mnk* (vv ff ex_pts)
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
	 (mtr        (make-instance 'matrix :dimensions  (list n m) :initial-element 0.0d0 ))
	 (mtr-lambda (make-instance 'matrix :dimensions  (list n m) :initial-element nil )))
    (dotimes (i n)
      (dotimes (j m)
	(matr-set-ij-*
	 mtr-lambda
	 (eval (list 'lambda  vv (cons '* (append (nth i ff) (nth j ff))))) i j)))
    
    (mapc
     #'(lambda (el)
	 (dotimes (i n)
	   (dotimes (j m)
	     (matr-set-ij-*
	      mtr
	      (+ (apply (matr-ij-* mtr-lambda i j) el)
		 (matr-ij-* mtr i j))
	      i j))))
     ex_pts)
    mtr))

(defmethod matr-triang* ((matr matrix ))
  "Выполняет приведение  матрицы  к треугольному виду, для решения системы ЛУ методом Гаусса;
Пример использования 1
 (matr-print 
 (matr-triang (matr-new 3 4 '(0.0 0.0 4.0 12.0
			      2.0 0.0 2.0 8.0
			      0.0 3.0 0.0 6.0))))
 (matr-triang* (make-instance 'matrix :initial-contents '((0.0 0.0 4.0 12.0)
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

 (matr-triang* (make-instance 'matrix :initial-contents 
			      '((1.0d0  2.0d0  3.0d0  4.0d0)
				(5.0d0  6.0d0  7.0d0  8.0d0)
				(9.0d0 10.0d0 11.0d0 12.0d0))))
=>
Matr 3 4
 (1.0D0 2.0D0 3.0D0 4.0D0)
 (-0.0D0 1.0D0 2.0D0 2.9999999999999996D0)
 (0.0D0 0.0D0 0.0D0 8.881784197001252D-16)
"
  (do ((n (matr-rows-* matr)) (ie nil) (j 0 (1+ j)) (row_j nil) (row_i nil))
      ((>= j n) matr)
    (setf ie (1- n))
    (do ((i j (1+ i)) (matr_ij nil) (row_ie nil)) ; Цикл перестановки строк в j-товом столбце которых присутстыуют нули
	((> i ie))
      (setf row_i   (matr-get-row-* matr i)
	    matr_ij (matr-ij-* matr i j))
      (cond ((= matr_ij 0) ; Перестановка i-товой строки в место поледней непереставленной
	     (setf row_ie (matr-get-row-* matr ie) ; Последняя непереставленная строка
		   matr (matr-set-row-* matr i row_ie) 
		   matr (matr-set-row-* matr ie row_i)
		   ie (1- ie)) ; Увеличение количества переставленных строк
	     (decf i)) ; Уменьшение переменной цикла для выполнения повторной итерации
	    ((/= matr_ij 0)
	     (setf row_i (mapcar #'(lambda (el) (/ el matr_ij)) row_i) ; Деление строки на matr_ij элемент матрицы
		   matr (matr-set-row-* matr i row_i)))))
    (setf row_j (matr-get-row-* matr j)) ; Строка которую необходимо вычесть из других строк
    (do ((i (1+ j)(1+ i))) ; Цикл для вычитания j-товой строки из i-товой
	((> i ie))			
      (setf row_i (matr-get-row-* matr i)
	    row_i (mapcar #'(lambda (el1 el2) (- el1 el2)) row_i row_j)
	    matr  (matr-set-row-* matr i row_i))
;;;;(matr-print matr)
      )))

(matr-new* 1 10)
(matr-new 1 10)

(defmethod matr-obrhod* ((matr matrix))
  "Обратный ход при вычислении решения системы линейных уравнений;
Матрица matr должна быть приведена к треуголной;"
  (let* ((n (matr-rows-* matr)) ; Количество строк в матрице (матрица расширенная)
	 (x (matr-new* 1 n))) ; Вектор результат
    (do ((i 0 (+ 1 i)))
	((>= i n) x)
      (setf x (matr-set-ij-* x 1 0 i)))
    (do ((i (- n 1) (- i 1)) (summ 0 0))
	((< i 0) x)
      (do ((j (+ 1 i) (+ 1 j)))
	  ((>= j  n) 'done2)
	(setf summ (+ summ (* (matr-ij-* matr i j) (matr-ij-* x 0 j)))))
      (setf x (matr-set-ij-* x (/ (- (matr-ij-* matr i n) summ) (matr-ij-* matr i i)) 0 i)))))

(defmethod matr-las-gauss* ((matr matrix))
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
	      (matr-get-row-* (matr-las-gauss* (matr-mnk* vv ff ex_pts)) 0)
	      ff)))
	(rez nil))
    (setf rez (list (reverse (cdr(reverse vv))) kk))
    rez))

(defun matr-osr-lambda* (vv ff ex_pts)
  "Пример использования:
  (matr-osr-lambda* '(xx yy) 
		 '((xx xx) (xx) (1.0) (yy)) 
		 '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0)))
;
=>(\"Matr\" 1 3 ((0 . 1.0d0) (1 . 0.0d0) (2 . 0.0d0)))
"
  (cons 'lambda (matr-osr-body* vv ff ex_pts)))


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


(defmethod matr-sum* ((a matrix ) (b matrix))
  "Выполняет сложение матриц a и b.
Пример использования:
 (matr-sum* (matr-new* 2 2 '(1 2 3 4)) (matr-new* 2 2 '(1 2 3 4)))
 (matr-sum* (matr-new* 2 2 '(1 2 3 4)) (matr-new* 2 2 '(4 3 2 1)))
"
  (assert (and (= (matr-rows-* a) (matr-rows-* b)) (= (matr-cols-* a) (matr-cols-* b))) (a b)
	  "Матрицы A[~A,~A] и B[~A,~A] имеют отличаюшиеся размеры"
	  (matr-rows-* a) (matr-cols-* a) (matr-rows-* b) (matr-cols-* b))
  (let ((a+b (matr-new* (matr-rows-* a) (matr-cols-* a))))
    (loop :for r :from 0 :below (matr-rows-* a) :do
	 (loop :for c :from 0 :below (matr-rows-* b) :do
	      (matr-set-ij-* a+b (+ (matr-ij-* a r c ) (matr-ij-* b r c ) ) r c) ))
    a+b))


(defmethod matr-mult* ((a matrix ) (b matrix))
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
  (let ((a_n (matr-rows-* a))
	(a_m (matr-cols-* a))
	(b_n (matr-rows-* b))
	(b_m (matr-cols-* b))
	(c nil))
    (assert (= a_m b_n) (a_m b_n) "Запрещенные размеры матриц для перемножения: A[~A,~A] x B[~A,~A]" a_n a_m b_n b_m)
    (setf c (make-instance 'matrix :dimensions (list a_n b_m) :initial-element 0.0))
    
    (do ((i 0 (1+ i))
	 (a-row nil))
	((>= i a_n))
      (setf a-row (matr-get-row-* a i))
      (do ((j 0 (1+ j))
	   (b-col nil))
	  ((>= j b_m))
	(setf b-col (matr-get-col-* b j))
	(setf c (matr-set-ij-* c (apply #'+ (mapcar #'* a-row b-col)) i j))))
    c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod matr-eval-* ((mm matrix))
  "Мутная функция и непонятно как ее использовать и где?"
  (let ((rows (matr-rows-* mm))
	(cols (matr-cols-* mm))
	(mm-cp (matr-copy-*  mm)))
    (loop :for i :from 0 :below rows
       :do
	 (loop :for j :from 0 :below cols
	    :do (setf (aref (matrix-data mm) i j) (eval (aref (matrix-data mm) i j)))))
    mm-cp))
