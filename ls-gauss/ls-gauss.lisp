;;;; /math/ls-gauss/ls-gauss.lisp

(defpackage #:math/ls-gauss
  (:use #:cl #:math/arr-matr)
  (:export convert-to-triangular
	   solve-linear-system-gauss-backward-run
	   solve-linear-system-gauss
	   ))

(in-package :math/ls-gauss)

(export 'convert-to-triangular
	)

(defmethod convert-to-triangular ((matr <matrix> ))
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

(export 'solve-linear-system-gauss-backward-run)

(defmethod solve-linear-system-gauss-backward-run ((matr <matrix>))
"Кандидат в intern.

 Обратный ход при вычислении решения системы линейных уравнений.
 Матрица matr должна быть приведена к треугольной;
 "  
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

(export 'solve-linear-system-gauss)

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
