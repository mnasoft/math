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

(defgeneric backward-run (matrix)
  (:documentation
   "Кандидат в intern.

 Обратный ход при вычислении решения системы линейных уравнений.
 Матрица matr должна быть приведена к треугольной;
 "))

(defgeneric convert-to-triangular (matrix)
  (:documentation
      "@b(Описание:) обобщенная_функция @b(convert-to-triangular) 
выполняет приведение  матрицы @b(matrix) к треугольному виду,
для решения системы ЛУ методом Гаусса."))

(defgeneric solve-x (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(solve-x)
возвращает матрицу, содержащую корни решения системы линейных уравений.

 Решение системы линейных уравнений выполняется методом Гаусса."))

(defmethod convert-to-triangular ((matr math/matr:<matrix>))
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
  (make-instance 'math/matr:<matrix> 
		 :initial-contents '((1.0d0  2.0d0  3.0d0  4.0d0)
				     (5.0d0  6.0d0  7.0d0  8.0d0)
				     (9.0d0 10.0d0 11.0d0 12.0d0))))
  => Matr 3х4
     [ 1.0d0     2.0d0     3.0d0     4.0d0    ]
     [ -0.0d0    1.0d0     2.0d0     2.9999999999999996d0 ]
     [ 0.0d0     0.0d0     0.0d0     8.881784197001252d-16 ]
@end(code)"
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
  "Кандидат в intern.

 Обратный ход при вычислении решения системы линейных уравнений.
 Матрица matr должна быть приведена к треугольной;
 "
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
  "@b(Пример использования 1.1:)
@begin[lang=lisp](code)
  (solve-x
   (math/matr:matr-new 
             3 4 '(1 2 3 14 
		   2 1 1 7 
		   3 0 1 2)))
 => Matr 1х3
    [ 1/3       16/3      1        ]
@end(code)

 @b(Пример использования 1.2:)
@begin[lang=lisp](code)
 (solve-x
  (math/matr:matr-new 
            3 4 '(1.0 2 3 14 
		  2   1 1  7 
		  3   0 1  2)))
 => Matr 1х3
    [ 0.33333397  5.333332  1.0000007 ]
@end(code)

 @b(Пример использования 2:)
@begin[lang=lisp](code)
 (solve-x 
   (math/matr:matr-new 
             3 4 '(1 0 1 4 
		   0 1 0 2 
		   0 0 1 3)))
  => Matr 1х3
     [ 1         2         3        ]
@end(code)"
  (let* ((matr-tr (convert-to-triangular matr))
	 (x (backward-run matr-tr)))
    x))

(defmethod solve-x ((matr cons))
  "@b(Пример использования:)
@begin[lang=lisp](code)
 (solve-x '((1 2 2)
           (3 5 4)))
  => #(-2 2)

@end(code)"
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
