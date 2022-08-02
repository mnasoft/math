(in-package :math/ls-gauss)


(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



(make-doc
  #'MATH/LS-GAUSS:SOLVE-X 'function
  "@b(Описание:) обобщенная_функция @b(solve-x)
возвращает матрицу, содержащую корни решения системы линейных уравений.

 Решение системы линейных уравнений выполняется методом Гаусса.
")

(make-doc
  #'MATH/LS-GAUSS:BACKWARD-RUN 'function
  "Кандидат в intern.

 Обратный ход при вычислении решения системы линейных уравнений.
 Матрица matr должна быть приведена к треугольной;
 ")

(make-doc
  #'MATH/LS-GAUSS:CONVERT-TO-TRIANGULAR 'function
  "@b(Описание:) обобщенная_функция @b(convert-to-triangular) 
выполняет приведение  матрицы @b(matrix) к треугольному виду,
для решения системы ЛУ методом Гаусса.")

(make-doc
  (find-method #'MATH/LS-GAUSS:CONVERT-TO-TRIANGULAR NIL '(MATH/ARR-MATR:<MATRIX>))
  t
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
  (make-instance 'math/arr-matr:<matrix> 
		 :initial-contents '((1.0d0  2.0d0  3.0d0  4.0d0)
				     (5.0d0  6.0d0  7.0d0  8.0d0)
				     (9.0d0 10.0d0 11.0d0 12.0d0))))
  => Matr 3х4
     [ 1.0d0     2.0d0     3.0d0     4.0d0    ]
     [ -0.0d0    1.0d0     2.0d0     2.9999999999999996d0 ]
     [ 0.0d0     0.0d0     0.0d0     8.881784197001252d-16 ]
@end(code)")

(make-doc
  (find-method #'MATH/LS-GAUSS:BACKWARD-RUN NIL '(MATH/ARR-MATR:<MATRIX>))
  t
  "Кандидат в intern.

 Обратный ход при вычислении решения системы линейных уравнений.
 Матрица matr должна быть приведена к треугольной;
 ")

(make-doc
  (find-method #'MATH/LS-GAUSS:SOLVE-X NIL '(MATH/ARR-MATR:<MATRIX>))
  t
  "@b(Пример использования 1.1:)
@begin[lang=lisp](code)
  (solve-x
   (math/arr-matr:matr-new 
             3 4 '(1 2 3 14 
		   2 1 1 7 
		   3 0 1 2)))
 => Matr 1х3
    [ 1/3       16/3      1        ]
@end(code)

 @b(Пример использования 1.2:)
@begin[lang=lisp](code)
 (solve-x
  (math/arr-matr:matr-new 
            3 4 '(1.0 2 3 14 
		  2   1 1  7 
		  3   0 1  2)))
 => Matr 1х3
    [ 0.33333397  5.333332  1.0000007 ]
@end(code)

 @b(Пример использования 2:)
@begin[lang=lisp](code)
 (solve-x 
   (math/arr-matr:matr-new 
             3 4 '(1 0 1 4 
		   0 1 0 2 
		   0 0 1 3)))
  => Matr 1х3
     [ 1         2         3        ]
@end(code)
")

