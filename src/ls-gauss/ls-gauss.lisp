;;;; ./src/ls-gauss/ls-gauss.lisp

(defpackage #:math/ls-gauss
  (:use #:cl) 
  (:export convert-to-triangular
	   backward-run
	   solve-x
           determiant
           singular-p
	   ))

(in-package :math/ls-gauss)

(defgeneric backward-run (matrix))

(defgeneric convert-to-triangular (matrix))

(defgeneric solve-x (matrix))

(defmethod convert-to-triangular ((matr math/matr:<matrix> ))
  (do ((n (math/matr:rows matr))
       (ie nil)
       (j 0 (1+ j))
       (row-j nil)
       (row-i nil)
       (determiant 1))
      ((>= j n)
       (values
        matr
        (* determiant (apply #'* (math/matr:main-diagonal matr)))))

    (setf ie (1- n))
    (do ((i j (1+ i)) (matr-ij nil) (row-ie nil)) ; Цикл перестановки строк в j-товом столбце которых присутстыуют нули
	((> i ie))
      (setf row-i   (math/matr:row matr i)
	    matr-ij (math/matr:mref matr i j))
      (cond ((= matr-ij 0) ; Перестановка i-товой строки в место поледней непереставленной
	     (setf row-ie (math/matr:row matr ie) ; Последняя непереставленная строка
		   (math/matr:row matr i) row-ie 
		   (math/matr:row matr ie) row-i
		   ie (1- ie)) ; Увеличение количества переставленных строк
	     (decf i)) ; Уменьшение переменной цикла для выполнения повторной итерации
	    ((/= matr-ij 0)
	     (setf row-i (mapcar #'(lambda (el) (/ el matr-ij)) row-i) ; Деление строки на matr-ij элемент матрицы
		   (math/matr:row matr i) row-i
                   determiant (* determiant matr-ij)))))
    (setf row-j (math/matr:row matr j)) ; Строка которую необходимо вычесть из других строк
    (do ((i (1+ j)(1+ i))) ; Цикл для вычитания j-товой строки из i-товой
	((> i ie))			
      (setf row-i (math/matr:row matr i)
	    row-i (mapcar #'(lambda (el1 el2) (- el1 el2)) row-i row-j)
	    (math/matr:row matr i) row-i))))

(defmethod convert-to-triangular ((matr cons))
  (convert-to-triangular
   (make-instance 'math/matr:<matrix>
                  :initial-contents matr)))

(defmethod backward-run ((matr math/matr:<matrix>))
  (let* ((n (math/matr:rows matr)) ;; Количество строк в матрице (матрица расширенная)
	 (x (math/matr:matr-new 1 n))) ;; Вектор результат
    (do ((i 0 (+ 1 i)))
	((>= i n) x)
      (setf (math/matr:mref x 0 i) 1 ))
    (do ((i (- n 1) (- i 1)) (summ 0 0))
	((< i 0) x)
      (do ((j (+ 1 i) (+ 1 j)))
	  ((>= j  n) 'done2)
	(setf summ (+ summ (* (math/matr:mref matr i j) (math/matr:mref x 0 j)))))
      (setf (math/matr:mref x 0 i) (/ (- (math/matr:mref matr i n) summ) (math/matr:mref matr i i))))))

(defmethod solve-x ((matr math/matr:<matrix>))
  (let* ((matr-tr (convert-to-triangular matr))
	 (x (backward-run matr-tr)))
    x))

(defmethod solve-x ((matr cons))
  (apply #'vector
  (math/matr:row
   (solve-x (make-instance 'math/matr:<matrix> :initial-contents matr))
   0)))

(defmethod determiant ((matr math/matr:<matrix>))
  (nth-value 1 (convert-to-triangular matr)))

(defmethod determiant ((matr cons))
  (determiant
   (make-instance 'math/matr:<matrix>
                  :initial-contents matr)))

(defmethod singular-p ((matr math/matr:<matrix>) &key (tolerance 1.0d-12))
  (math/core:semi-equal (determiant matr) 0 :tolerance tolerance))

(defmethod singular-p ((matr cons) &key (tolerance 1.0d-12))
  (math/core:semi-equal (determiant matr) 0 :tolerance tolerance))
