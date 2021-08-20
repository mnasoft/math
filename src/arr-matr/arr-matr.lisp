;;;; /src/arr-matr/package.lisp

(defpackage #:math/arr-matr
  (:use #:cl) ;;#:math/core
  (:export matr-new
	   <matrix>
           )
  (:export swap-cols
           swap-cols*
           swap-rows
           swap-rows*)
  (:export initialize-instance
           matrix-data
           convert-to-triangular
           matrix->2d-list
           transpose 
           matr-eval-*
           )
  (:export *semi-equal-relativ*
           *semi-equal-zero*
           semi-equal
           equivalent)
  (:export mref
           copy
           dimensions
           rows
           cols
           row
           col
           main-diagonal
           squarep
           anti-diagonal
           add
           multiply)
  #+nil
  (:export math/core:mref
   	   math/core:col
           math/core:row
	   math/core:cols
   	   math/core:rows
           math/core:equivalent
           math/core:add
           math/core:multiply
           math/core:main-diagonal
           math/core:dimensions
           math/core:squarep
           math/core:anti-diagonal
           math/core:copy
           )
  )

(in-package #:math/arr-matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric matr-eval-* (matrix) (:documentation "Matr"))

(defgeneric matr-equal* (matrix1 matrix2 &key test) (:documentation "Проверка матриц на равенство"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmethod matr-name-* ((mm <matrix>))
  "@b(Описание:) функция|метод|обобщенная_функция| @b(...) "
  "Matr")

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

(defmethod mref ((mm <matrix>) i j) (aref (matrix-data mm) i j))

(defmethod (setf mref) (value (mm <matrix>) i j) (setf (aref (matrix-data mm) i j) value) mm)

(defmethod copy ((mm-ref <matrix>))
  (make-instance '<matrix> :data (cl-utilities:copy-array (matrix-data mm-ref))))

(defmethod dimensions ((mm <matrix>)) (array-dimensions (matrix-data mm)))

(defmethod rows ((mm <matrix>)) (array-dimension (matrix-data mm) 0))

(defmethod cols ((mm <matrix>)) (array-dimension (matrix-data mm) 1))

;;;;;;;;;; equivalent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *semi-equal-relativ* 1e-6
  "@b(Описание:) переменная @b(*semi-equal-relativ*) определяет
относительную величину, на которую могут отличаться значения 
считающиеся равными при срвнении из с помощью функции @b(semi-equal).
")

(defparameter *semi-equal-zero*    1e-6
    "@b(Описание:) переменная @b(*semi-equal-zero*) определяет
абсолютную величину, на которую могут отличаться значения 
считающиеся равными при срвнении из с помощью функции @b(semi-equal).
")

(defun semi-equal (x y
		   &key (tolerance
			 (+
			  (*
			   *semi-equal-relativ*
			   (sqrt (+ (* y y) (* x x))))
			  *semi-equal-zero*))) 
  "@b(Описание:) функция @b(semi-equal) возвращает T, если 
расстояние между значениями меньше tolerance. При этом 
имеется в виду, что значения примерно равны.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (semi-equal 1.0 1.000001)
@end(code)
"
  (labels ((distance (x y) (abs (- x y))))
    (< (distance x y) tolerance)))

(defmethod equivalent ((m1 <matrix>) (m2 <matrix>) &key (test #'semi-equal))
  (let ((rez t))
    (if (and (= (rows m1) (rows m2))
	     (= (cols m1) (cols m2)))
	(loop :for r :from 0 :below (rows m1) :do
	   (loop :for c :from 0 :below (cols m1) :do
		(setf rez (and rez (funcall test (mref m1 r c ) (mref m2 r c) )))))
	(setf rez nil))
    rez))

(defmethod equivalent ((a1 array) (a2 array) &key (test #'semi-equal))
   (declare (type (array * (* *)) a1 a2))
   (when (not (equal (array-dimensions a1)
		     (array-dimensions a2)))
     (return-from equivalent nil))
   (reduce #'(lambda (el1 el2) (and el1 el2))
	   (apply #'append
		  (loop :for i :from 0 :below (array-dimension a1 0)
			:collect
			(loop :for j :from 0 :below (array-dimension a1 1)
			      :collect
			      (funcall test (aref a1 i j) (aref a2 i j)))))
	   :initial-value t))

(defmethod equivalent ((m1 <matrix>) (a2 array) &key (test #'semi-equal))
  (declare (type (array * (* *)) a2))  
  (equivalent m1 (make-instance '<matrix> :data a2) :test test))

(defmethod equivalent ((a1 array) (m2 <matrix>) &key (test #'semi-equal))
  (declare (type (array * (* *)) a1))
  (equivalent (make-instance '<matrix> :data a1) m2 :test test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmethod (setf main-diagonal) (elements (mm <matrix>))
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
  (loop :for i :from 0 :below (min (rows mm) (cols mm))
	:for el :in elements :by #'cdr  :do
	  (setf (mref  mm i i) el))
  mm)

(defmethod (setf main-diagonal) ((element number) (mm <matrix>) )
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
  (loop :for i :from 0 :below (min (rows mm) (cols mm))
	:do (setf (mref  mm i i) element))
  mm)

(defmethod squarep ((mm <matrix>))
  "@b(Описание:) метод @b(squarep) возвращает T, если матрица является
  квадратной. В противном случае возвращает nil.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (defparameter *mm* (make-instance '<matrix>   :dimensions '(2 3)))
  (squarep *mm*) => nil

  (defparameter *mm* (make-instance '<matrix>   :dimensions '(3 3)))
  (squarep *mm*) => T
@end(code)
"
  (= (cols mm) (rows mm) ))

(defmethod anti-diagonal ((mm <matrix>))
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

(defmethod add ((a <matrix> ) (b <matrix>))
  "@b(Описание:) метод @b(add) возвращает матрицу типа <matrix>,
являющуюся результатом сложения матриц a и b.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (add (matr-new 2 2 '(1 2 
                      3 4)) 
      (matr-new 2 2 '(1 2 
                      3 4)))
  => Matr 2х2
     [ 2         4        ]
     [ 6         8        ]
 (add (matr-new 2 2 '(1 2 3 4)) (matr-new 2 2 '(4 3 2 1)))
  => Matr 2х2
     [ 5         5        ]
     [ 5         5        ]
@end(code)
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

(defmethod multiply ((a <matrix> ) (b <matrix>))
  "@b(Описание:) метод @b(multiply) возвращает матрицу типа <matrix>,
являющуюся результатом умножения матриц @b(a) и @b(b).

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

(defmethod multiply ((a number ) (b <matrix>))
    "@b(Описание:) метод @b(multiply) возвращает матрицу типа (<matrix>),
     являющуюся результатом умножения числа @b(a) и матрицы @b(b).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (multiply 10 
           (matr-new 2 3 '(1.0 2.0 3.0
			   4.0 5.0 6.0)))
 => Matr 2х3
    [ 10.0      20.0      30.0     ]
    [ 40.0      50.0      60.0     ]
@end(code)
"
  (let ((rez (make-instance '<matrix> :dimensions (dimensions b))))
    (loop :for i :from 0 :below (rows b) :do
	 (loop :for j :from 0 :below (cols b) :do
	      (setf (mref rez i j) (* a (mref b i j)))))
    rez))

(defmethod matrix->2d-list ((mm <matrix>))
  (loop :for i :from 0 :below (rows mm) :collect
					(row mm i)))

(defmethod transpose ((mm <matrix>))
  "@b(Описание:) метод @b(transpose) возвращает матрицу типа <matrix>,
   являющуютя результатом транспонирования матрицы @b(mm).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (transpose (matr-new 2 3 '(1 2 3
                            4 5 6)))
  => Matr 3х2
     [ 1         4        ]
     [ 2         5        ]
     [ 3         6        ]
@end(code)"
  (let ((rez (make-instance '<matrix> :dimensions (nreverse (dimensions mm)))))
    (loop :for i :from 0 :below (rows mm) :do
	 (loop :for j :from 0 :below (cols mm) :do
	      (setf (mref rez j i) (mref mm i j))))
    rez))

(defmethod transpose ((mm cons))
  "Выполняет транспонирование"
  (apply #'mapcar #'list mm))

(defmethod swap-rows*  ((mm <matrix>) i j)
  (assert (and (< -1 i (rows mm)) (< -1 j (rows mm))))
  (when (/= i j)
    (let ((row-i (row mm i))
	  (row-j (row mm j)))
      (setf (row mm i) row-j
	    (row mm j) row-i)))
  mm)

(defmethod swap-cols*  ((mm <matrix>) i j)
  (assert (and (< -1 i (cols mm)) (< -1 j (cols mm))))
  (when (/= i j)
    (let ((col-i (col mm i))
	  (col-j (col mm j)))
      (setf (col mm i) col-j
	    (col mm j) col-i)))
  mm)

(defmethod swap-rows  ((mm <matrix>) i j)
  (swap-rows* (copy mm) i j))

(defmethod swap-cols  ((mm <matrix>) i j)
  (swap-cols* (copy mm) i j))

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
