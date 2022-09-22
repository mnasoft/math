;;;; ./src/matr/matr.lisp

(defpackage #:math/matr
  (:use #:cl #:math/coord)
  (:export initialize-instance)
  (:export matr-new
           make-vector-n
	   <matrix>
           )
  (:export swap-cols
           swap-cols*
           swap-rows
           swap-rows*)
  (:export matrix-data
           matrix->2d-list
           transpose 
           matr-eval-*
           )
  (:export equivalent)
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
  (:export average-value
           average-not-nil-value
           average-row-value
           average-row-not-nil-value
           average-col-value
           average-col-not-nil-value
           )
  (:export max-row-not-nil-value
           max-col-not-nil-value)
  (:export detach-last-col
           get-last-col)
  (:export prepend-row
           prepend-rows
           prepend-col)
  (:export append-row
           append-col)
  (:export lv-print
           lm-print)
  (:export unite-rows
           unite-cols)
  (:export make)
  (:export normalize
           rotate-x
           rotate-y
           rotate-z
           rotate-v
           rotate-around
           move-xyz
           move-v
           transform)
  (:documentation
   "@b(Описание:) пакет @b(math/matr) определяет некоторые операции
 над матрицами, представленными 2d-list, (списком состоящим из
 списков)")
  )

(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; /src/arr-matr/package.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric matr-eval-* (matrix)
  (:documentation
   "Matr"))

(defgeneric matr-equal* (matrix1 matrix2 &key test) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; transpose

(defgeneric transpose (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(transpose) 
возвращает транспонированную матрицу."))

(defgeneric matr-name-* (matrix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <matrix> ()
  ((data :accessor matrix-data :initform nil :initarg :data :documentation
         "Сдержимое матрицы."))
  (:documentation
   "@b(Описание:) класс @b(<matrix>) представляет матрицу, определенную
 через массив.

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
@end(code)"))

(defmethod matr-name-* ((mm <matrix>))
  (type-of mm))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; copy
(defgeneric copy (obj)
  (:documentation
    "@b(Описание:) обобщенная_функция @b(copy) возвращает ссылку на новый объект,
созданный на основе @b(obj)."))

(defmethod copy ((mm-ref <matrix>))
  (make-instance '<matrix> :data (cl-utilities:copy-array (matrix-data mm-ref))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; dimensions
(defgeneric dimensions (matrix)
  (:documentation
    "@b(Описание:) обобщенная_функция @b(dimensions) возвращает список,
 содержащий размерности матрицы @b(matrix)."))

(defmethod dimensions ((mm <matrix>))
  (array-dimensions (matrix-data mm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; equivalent

(defgeneric equivalent (matrix-1 matrix-2 &key test)
  (:documentation
     "@b(Описание:) обобщенная_функция @b(equivalent) возвращает T,
если матирицы @b(matrix-1) и @b(matrix-2) имеют одинаковые размерности и их 
соответствующие элементы равны (для них функция @b(test) возвращает T )."))
(defmethod equivalent ((m1 <matrix>) (m2 <matrix>) &key (test #'math/core:semi-equal))
  (let ((rez t))
    (if (and (= (rows m1) (rows m2))
	     (= (cols m1) (cols m2)))
	(loop :for r :from 0 :below (rows m1) :do
	   (loop :for c :from 0 :below (cols m1) :do
		(setf rez (and rez (funcall test (mref m1 r c ) (mref m2 r c) )))))
	(setf rez nil))
    rez))

(defmethod equivalent ((a1 array) (a2 array) &key (test #'math/core:semi-equal))
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

(defmethod equivalent ((m1 <matrix>) (a2 array) &key (test #'math/core:semi-equal))
  (declare (type (array * (* *)) a2))  
  (equivalent m1 (make-instance '<matrix> :data a2) :test test))

(defmethod equivalent ((a1 array) (m2 <matrix>) &key (test #'math/core:semi-equal))
  (declare (type (array * (* *)) a1))
  (equivalent (make-instance '<matrix> :data a1) m2 :test test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; main-diagonal

(defgeneric main-diagonal (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(main-diagonal) извлекает главную
 диагональ матрицы.

Элементы возвращаются в порядке возрастания строк."))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (setf main-diagonal)
(defgeneric (setf main-diagonal) (elements matrix)
   (:documentation "@b(Описание:) обобщенная_функция @b((setf main-diagonal))
 устанавливает новые значения элементам матрицы @b(matrix),
 находящимся на главной диагонали.

 Элементы @b(elements) устанавливаются в порядке возрастания строк."))

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
  " @b(Пример использования:)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; squarep
(defgeneric squarep (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(squarep) возвращает T, 
если матрица @b(matrix) является квадратной."))

(defmethod squarep ((mm <matrix>))
  " @b(Пример использования:)
@begin[lang=lisp](code)
  (defparameter *mm* (make-instance '<matrix>   :dimensions '(2 3)))
  (squarep *mm*) => nil

  (defparameter *mm* (make-instance '<matrix>   :dimensions '(3 3)))
  (squarep *mm*) => T
@end(code)
"
  (= (cols mm) (rows mm) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; anti-diagonal

(defgeneric anti-diagonal (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(anti-diagonal)
возвращает список элементов, находящихся на побочной диагонали матрицы.

 В результирующем списке элементы следуют по строкам.

 Д.б опредена только для квадратной матрицы."))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (setf anti-diagonal)

(defgeneric (setf anti-diagonal) (elements matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b((setf anti-diagonal)) устанавливет 
новые значения элементам матрицы @b(matrix), на побочной диагонали матрицы.

 Элементы @b(elements) устанавливаются в порядке возрастания строк."))

(defmethod (setf anti-diagonal) (elements (mm <matrix>) )
  (assert (squarep mm) (mm) "Матрица не является квадратной~%~S" mm)
  (loop
     :for c :from 0 :below (cols mm)
     :for r :downfrom (- (rows mm) 1) :to 0
     :for e :in elements :by #'cdr :do
       (setf (mref mm c r) e))
  mm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; add
(defgeneric add (a b)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(add) выполняет сложение
 аргументов @b(a) и @b(b)."))

(defmethod add ((a <matrix> ) (b <matrix>))
  " @b(Пример использования:)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; multiply
(defgeneric multiply (a b)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(multiply) выполняет перемножение
 аргументов @b(a) и @b(b)."))

(defmethod multiply ((a <matrix> ) (b <matrix>))
  " @b(Пример использования:)
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
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (multiply 10 
           (matr-new 2 3 '(1.0 2.0 3.0
			   4.0 5.0 6.0)))
 => Matr 2х3
    [ 10.0      20.0      30.0     ]
    [ 40.0      50.0      60.0     ]
@end(code)"
  (let ((rez (make-instance '<matrix> :dimensions (dimensions b))))
    (loop :for i :from 0 :below (rows b) :do
	 (loop :for j :from 0 :below (cols b) :do
	      (setf (mref rez i j) (* a (mref b i j)))))
    rez))

(defmethod matrix->2d-list ((mm <matrix>))
  (loop :for i :from 0 :below (rows mm) :collect
					(row mm i)))



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; /src/2d-array/2d-array.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-vector-n (element n)
    "
 @b(Пример использования:)
@begin[lang=lisp](code)

@end(code)

 (make-vector-n 1.5 3) => #(1.5 1.5 1.5)"
  (make-array (list n) :initial-element element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ./src/list-matr/list-matr.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unite-rows (2d-list)
  "@b(Описание:) функция @b(unite-rows) объединяет строки матрицы
 (список списков) в вектор (список).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (unite-rows '((1 2 3)
               (4 5 6)))
 =>(1 2 3 4 5 6)
@end(code)"  
  (let ((rez nil))
    (mapcar #'(lambda (el) (setf rez (append rez el) ) ) 2d-list)
    rez))

(defun unite-cols (2d-list)
  (let ((rez nil))
    (mapcar #'(lambda (el) (setf rez (append rez el) ) ) 2d-list)
    rez))

(defun make (rows cols 2d-list)
  "@b(Описание:) функция @b(make) генерирует матрицу 
(список списков) из вектора (списка).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (math:make 2 3 '(0 1 2 3 4 5 6 7 8 9 10 11 12))
 => ((0 1 2) 
     (3 4 5))
 (math:make 2 3 'nil)
 => ((NIL NIL NIL) 
     (NIL NIL NIL))
@end(code)"  
  (let ((rez nil)
        (rw nil))
    (dotimes (r rows (reverse rez))
      (dotimes (c cols)
        (push (nth (+ (* r cols) c) 2d-list) rw))
      (push (reverse rw) rez)
      (setf rw nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; average - Вычисление среднего значения
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun average-value (2d-list)
  "@b(Описание:) функция @b(average-value) вычисляет среднее значение по
 элементам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-value '((1.0 1.5 2.0)
                  (2.0 2.5 3.0))) => 2.0
@end(code)"  
  (math/stat:average-value (unite-rows 2d-list)))

(defun average-not-nil-value (2d-list)
  "@b(Описание:) функция @b(average-not-nil-value) вычисляет среднее
 значение по элементам матрицы (списка списков) с исключением
 nil-элементов."  
  (math/stat:average-not-nil-value (unite-rows 2d-list)))

;;;;

(defun average-row-value (lst)
  "@b(Описание:) функция @b(average-row-value) вычисляет 
средние значения в строках матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-row-value '((1.0 1.5 2.0)
                      (2.0 2.5 3.0))) => (1.5 2.5)
@end(code)"  
  (mapcar #'math/stat:average-value lst))

(defun average-row-not-nil-value (lst)
  "@b(Описание:) функция @b(average-row-not-nil-value) вычисляет среднее
 значение по элементам матрицы (списка списков)."
  (mapcar #'math/stat:average-not-nil-value lst))

(defun max-row-not-nil-value (lst)
  "@b(Описание:) функция @b(max-row-not-nil-value) вычисляет максимальные 
значения по строкам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (max-row-not-nil-value '((1.0 2.1 1.5 2.0)
                          (2.0 2.5 3.2 3.0))) => (2.1 3.2)
@end(code)"  
  (mapcar #'(lambda (el) (apply #'max (math/core:exclude-nil-from-list el))) lst))

;;;;

(defun average-col-value (lst)
  "@b(Описание:) функция @b(average-col-value) вычисляет среднее
значение по столбцам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-col-value '((3.0 2.1 4.5)
                     (2.0 2.5 3.2))) => (2.5 2.3 3.85)
@end(code)"  
  (average-row-value (transpose lst)))

(defun average-col-not-nil-value (lst)
  "@b(Описание:) функция @b(average-col-not-nil-value) вычисляет среднее 
значение по элементам матрицы (списка списков).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-col-not-nil-value '((nil 2.1 4.5)
                             (2.0 nil 3.2))) => (2.0 2.1 3.85)
  
@end(code)"  
  (average-row-not-nil-value (transpose lst)))

(defun max-col-not-nil-value (lst)
  "@b(Описание:) функция @b(max-col-not-nil-value) вычисляет среднее
 значение по столбцам матрицы (списка списков)."  
  (max-row-not-nil-value (transpose lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; lm-print

(defun lm-print (2d-list &key (fmt "~6,1f") (stream t))
  "@b(Описание:) функция @b(lm-print) красивый вывод матрицы (списка
 списков)."  
  (format stream (concatenate 'string  "~{~{" fmt "~^ ~}~%~}") 2d-list))

(defun lv-print (lst &key (fmt "~6,1f") (stream t))
  "@b(Описание:) функция @b(lv-print) красивый вывод
 вектора (списка)."  
  (format stream (concatenate 'string  "~{" fmt "~^ ~}~%") lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; append

(defun append-col (c-lst 2d-list)
  "@b(Описание:) функция @b(append-col) добавляет к матрице,
 представленной списком 2d-list, столбец (список) c-lst.

 @b(Пример использования:) @begin[lang=lisp](code)
 (math:append-col '(10 11 12)
                  '((1 2 3)
                    (4 5 6)
                    (7 8 9))) => ((1 2 3 10) 
                                  (4 5 6 11)
                                  (7 8 9 12))
@end(code)"  
  (let ((rez nil)
        (r nil))
    (dolist (l 2d-list (reverse rez))
      (setf r (car c-lst)
            c-lst (cdr c-lst))
      (push (append l (list r)) rez))))

(defun append-row (c-lst 2d-list)
  "@b(Описание:) функция @b(append-row) добавляет вектор (список) r-lst
 к матрице 2d-list.

 @b(Пример использования:) @begin[lang=lisp](code)
 (math:append-row '(10 11 12)
                  '((1 2 3)
                    (4 5 6)
                    (7 8 9)))  =>((1 2 3)
                                  (4 5 6)
                                  (7 8 9)
                                  (10 11 12))

 (math:append-row '(10 11 )
                  '((1 2 3)
                    (4 5 6)
                    (7 8 9)))
 =>((1 2 3)
    (4 5 6)
    (7 8 9)
    (10 11 NIL))

 (math:append-row '(10 11 12 13)
                  '((1 2 3)
                    (4 5 6)
                    (7 8 9))) =>(( 1  2  3)
                                 ( 4  5  6)
                                 ( 7  8  9)
                                 (10 11 12))
@end(code)"  
  (transpose (append-col c-lst (transpose 2d-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; prepend

(defun prepend-col (c-lst 2d-list)
 "@b(Описание:) функция @b(prepend-col) добавляет вектор
(список) c-lst к матрице 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (prepend-col '(10 11 12)
              '((1  2  3)
                (4  5  6)
                (7  8  9))) => ((10 1 2 3)
                                (11 4 5 6)
                                (12 7 8 9))

@end(code)"  
  (let ((rez nil)
        (r nil))
    (dolist (l 2d-list (reverse rez))
      (setf r (car c-lst)
            c-lst (cdr c-lst))
      (push (cons r l) rez))))

(defun prepend-row (c-lst 2d-list)
  "@b(Описание:) функция @b(prepend-row)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (prepend-row '(10 11 12) 
              '((1 2 3)
                (4 5 6)
                (7 8 9))) =>((10 11 12) 
                             ( 1  2  3) 
                             ( 4  5  6)
                             ( 7  8  9))
 (math:prepend-row '(10 11 )
                   '((1 2 3)
                     (4 5 6) 
                     (7 8 9))) =>((10 11 NIL) 
                                  (1 2 3) 
                                  (4 5 6) 
                                  (7 8 9))
 (math:prepend-row '(10 11 12 13)
                   '((1 2 3)
                     (4 5 6)
                     (7 8 9)))  =>((10 11 12)
                                   ( 1  2  3) 
                                   ( 4  5  6) 
                                   ( 7  8  9))
@end(code)"
  (transpose (prepend-col c-lst (transpose 2d-list))))

(defun prepend-rows (rs-lst 2d-list)
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (prepend-rows
 '((10 20 30)
   (11 22 33))
 '((11 12 13)
   (12 13 14)
   (13 14 15)))
@end(code)"  
  (reduce #'(lambda (x y) (prepend-row y x)) (reverse rs-lst) :initial-value 2d-list))

(defun detach-last-col (2d-list)
  "@b(Описание:) функция @b(detach-last-col) возвращает матрицу, 
представленную в виде списка, образованную удалением последнего столбца 
(последнего элемента каждой строки).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (detach-last-col
  '((1 2 3  6)
    (2 3 4  9) 
    (3 4 5 12))) => ((1 2 3)
                     (2 3 4)
                     (3 4 5))
@end(code)"  
  (mapcar
   #'(lambda(el) (reverse (cdr (reverse el))))
   2d-list))

(defun get-last-col (2d-list)
  "@b(Описание:) функция @b(get-last-col) возвращает последний столбец
 матрицы, представленной в виде списка.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (get-last-col
 '((1  2  3  6)
   (2  3  4  9) 
   (3  4  5 12))) =>(6 9 12) 
@end(code)
"  
  (mapcar
   #'(lambda(el) (car (last el)))
   2d-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ./src/transform/transform.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun normalize (v)
  "@b(Описание:) функция @b(normalize) возвращает нормализованный вектор.
Длина нормализованного вектора равна 1.0.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (normalize '(1 2 3)) => (0.26726124 0.5345225 0.8017837)
 (normalize '(2 -3))  => (0.5547002 -0.8320503)
@end(code)"  
  (let ((len (sqrt (apply #'+ (mapcar #'(lambda (el) (* el el)) v)))))
    (mapcar #'(lambda (el) (/ el len)) v)))

(defgeneric rotate-x (α)
  (:documentation
     "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
  преобразования, которая вращает систему координат на угол α вокруг
  оси x."))

(defmethod rotate-x ((α number))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (progn (defparameter *p* (make-instance 'math/matr:<matrix> :dimensions '(1 4)))
        (setf (math/matr:row *p* 0) '(10.0 20.0 30.0 1.0))
        (math/matr:multiply *p* (rotate-y (dtr 90.0))))
@end(code)
"
  (let ((matrix (make-instance '<matrix> :dimensions '(4 4))))
    (setf (row matrix 0) `(1.0     0.0         0.0   0.0))
    (setf (row matrix 1) `(0.0 ,(cos α) ,(- (sin α)) 0.0))
    (setf (row matrix 2) `(0.0 ,(sin α)    ,(cos α)  0.0))
    (setf (row matrix 3) `(0.0     0.0         0.0   1.0))
    matrix))

(defmethod rotate-y ((β number))
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
  преобразования, которая вращает систему координат на угол β вокруг
  оси y."
  (let ((matrix (make-instance '<matrix> :dimensions '(4 4))))
    (setf (row matrix 0) `(   ,(cos β)  0.0 ,(sin β) 0.0))
    (setf (row matrix 1) `(       0.0   1.0     0.0  0.0))
    (setf (row matrix 2) `(,(- (sin β)) 0.0 ,(cos β) 0.0))
    (setf (row matrix 3) `(       0.0   0.0     0.0  1.0))
    matrix))

(defmethod rotate-z ((γ number))
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая вращает систему координат на угол γ вокруг оси
z."
  (let ((matrix (make-instance '<matrix> :dimensions '(4 4))))
    (setf (row matrix 0) `(,(cos γ) ,(- (sin γ)) 0.0 0.0))
    (setf (row matrix 1) `(,(sin γ)    ,(cos γ)  0.0 0.0))
    (setf (row matrix 2) `(    0.0         0.0   1.0 0.0))
    (setf (row matrix 3) `(    0.0         0.0   0.0 1.0))
    matrix))

(defmethod rotate-v ((θ number) (v cons))
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая вращает систему координат на угол α вокруг
оси, заданной вектором v. Вектор v должен быть нормализованным (иметь
единичную длину)."
  (let ((matrix (make-instance '<matrix> :dimensions '(4 4)))
        (x (first  v))
        (y (second v))
        (z (third  v)))
    (setf (row matrix 0) `(,(+       (cos θ) (* (- 1 (cos θ)) x x)) ,(- (* (- 1 (cos θ)) x y) (* (sin θ) z)) ,(+ (* (- 1 (cos θ)) x z) (* (sin θ) y)) 0.0))
    (setf (row matrix 1) `(,(+ (* (- 1 (cos θ)) y x) (* (sin θ) z))       ,(+ (cos θ) (* (- 1 (cos θ)) y y)) ,(- (* (- 1 (cos θ)) y z) (* (sin θ) x)) 0.0))
    (setf (row matrix 2) `(,(- (* (- 1 (cos θ)) z x) (* (sin θ) y)) ,(+ (* (- 1 (cos θ)) z y) (* (sin θ) x))       ,(+ (cos θ) (* (- 1 (cos θ)) z z)) 0.0))
    (setf (row matrix 3) `(                                     0.0                                      0.0                                      0.0 1.0))
    matrix))

(defmethod move-xyz ((dx number) (dy number) (dz number))
    "@b(Описание:) метод @b(move-xyz) возвращает однородную матрицу
преобразования, которая перемещает систему координат на (dx dy dz).
"
  (let ((matrix (make-instance '<matrix> :dimensions '(4 4))))
    (setf (row matrix 0) `(1.0 0.0 0.0 0.0))
    (setf (row matrix 1) `(0.0 1.0 0.0 0.0))
    (setf (row matrix 2) `(0.0 0.0 1.0 0.0))
    (setf (row matrix 3) `(,dx ,dy ,dz 1.0))
    matrix))

(defmethod move-v ((v cons))
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая перемещает систему координат в направлении
вектора v."
  (move-xyz (first v) (second v) (third v)))

(defmethod rotate-around ((point-1 cons) (point-2 cons) θ)
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая вращает протсранство вокруг оси, заданной
точками point-1 и point-2"
  (let ((rotate-p1-p2 (rotate-v θ (normalize (mapcar #'- point-2 point-1))))
        (move-p1-p0   (move-v (mapcar #'- point-1)))
        (move-p0-p1   (move-v point-1)))
    (multiply
     (multiply move-p1-p0 rotate-p1-p2)
     move-p0-p1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; transform

(defgeneric transform (point matrix)
  (:documentation
   "@b(Описание:) метод @b(transform) возвращает координаты точки
  @b(point), преобразованные с помощью матрицы @b(matrix)."))

(defmethod transform ((point cons) (matrix <matrix>))
"
 @b(Пример использования:)
@begin[lang=lisp](code)
 (To-Do) 
@end(code)
"
  (let ((p (make-instance '<matrix> :dimensions '(1 4))))
    (setf (row p 0) (list (first point) (second point) (third point) 1.0))
    (nreverse (cdr (nreverse (row (multiply p matrix) 0))))))


