;;;; ./src/matr/matr.lisp

(defpackage :math/matr
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
над матрицами, представленными в виде:
@begin(list)
 @item(2d-list - списком состоящим из списков;)
 @item(array   - массив c размерностью 2d;)
 @item(<matrix> - матрица c размерностью 2d.)
@end(list)"))

(in-package :math/matr)

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
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (matr-name-* (matr-new 3 2))
@end(code) "
  (type-of mm))

(defmethod print-object ((matrix <matrix>) stream)
  (print-unreadable-object (matrix stream :type t)
    (when (and (matrix-data matrix) (arrayp (matrix-data matrix)))
      (format stream "~{~A~^х~}" (array-dimensions (matrix-data matrix)))
      (loop :for i :from 0 :below (array-dimension (matrix-data matrix) 0)
            :do
	       (format stream "~%")
	       (loop :for j :from 0 :below (array-dimension (matrix-data matrix) 1)
	             :do (format stream " ~6A" (aref (matrix-data matrix) i j)))))))

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
  "@b(Описание:) функция @b(matr-new) возвращает объект <matrix> с числом
строк @b(rows) и числом столбцов @b(cols). Если опциональный аргумент
@b(lst) отличается от nil элементы марицы инициализируются значениями,
находящимися в списке @(lst) построчно.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (matr-new 3 4 '(1 2 3 4 5 6 7 8 9 10))
 #<<MATRIX> 3х4
  1  2  3   4     
  5  6  7   8     
  9  10 NIL NIL >
@end(code)"
  (assert (listp lst))
  (let ((mm (make-instance '<matrix> :dimensions (list rows cols) :initial-element 0.0d0))
	(ll lst))
    (when (consp lst)
	(loop :for i :from 0 :below (array-dimension (matrix-data mm) 0) :do
	     (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1) :do
		  (setf (aref (matrix-data mm) i j) (car ll)
			ll (cdr ll)))))
    mm))

(defmethod matrix->2d-list ((mm <matrix>))
  (loop :for i :from 0 :below (rows mm) :collect
					(row mm i)))

(defmethod swap-cols*  ((mm <matrix>) i j)
  (assert (and (< -1 i (cols mm)) (< -1 j (cols mm))))
  (when (/= i j)
    (let ((col-i (col mm i))
	  (col-j (col mm j)))
      (setf (col mm i) col-j
	    (col mm j) col-i)))
  mm)



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
    "@b(Описание:) функция @b(make-vector-n) возвращает вектор, в котором
содержится @b(n) элеметро @b(element).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-vector-n 1.5 3) => #(1.5 1.5 1.5)
@end(code)"
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

#+nil (defmethod transform ((point cons) (matrix <matrix>))
"
 @b(Пример использования:)
@begin[lang=lisp](code)
 (To-Do) 
@end(code)
"
  (let ((p (make-instance '<matrix> :dimensions '(1 4))))
    (setf (row p 0) (list (first point) (second point) (third point) 1.0))
    (nreverse (cdr (nreverse (row (multiply p matrix) 0))))))


(defmethod transform ((point-3d cons) (matrix-4x4 <matrix>))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (To-Do) 
@end(code)
"
  (math/coord:single-float-list
     (math/coord:point-4d->3d
      (math/matr:row 
       (math/matr:multiply
        (math/matr:matr-new 1 4 (math/coord:point-3d->4d point-3d)) matrix-4x4)
       0))))
