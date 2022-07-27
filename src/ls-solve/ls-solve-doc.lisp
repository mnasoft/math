;;;; ./src/ls-solve/lu-solve-doc.lisp

(in-package #:math/ls-solve)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



(make-doc
  #'MATH/LS-SOLVE:LU-SOLVE-EXTMATR 'function
  "@b(Описание:) функция| @b(lu-solve) возвращает корни решения СЛАУ
 (системы линейных алгебраических уравнений), используя LU-разложение
 матрицы @b(m-v).
 
 @b(Переменые:)
@begin(list)
 @item(m-v - матрица, представленная в виде 2d-list, (списком
       состоящим из списков));
@end(list)

  @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((m '((1 2 3   14)  
	    (2 1 1    7)  
	    (3 0 1    6))))
   (lu-solve-extmatr m :grid-type 'array))
=> #m(1.000000000000000d0  2.000000000000000d0  3.000000000000000d0)
=> #(1.0000000000000002d0 2.0000000000000004d0 2.9999999999999996d0)
  ")

(make-doc
  #'MATH/LS-SOLVE:LU-SOLVE 'function
  "@b(Описание:) функция| @b(lu-solve) возвращает корни решения СЛАУ 
(системы линейных алгебраических уравнений), 
используя LU-разложение матрицы @b(matrix).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((m (grid:make-foreign-array
           'double-float :initial-contents
           '((1 2 3)  
	     (2 1 1)  
	     (3 0 1)))) 
       (v (grid:make-foreign-array
           'double-float :initial-contents
           '(14 7 6))))
   (lu-solve m v)) => 
#m(1.000000000000000d0 2.000000000000000d0 3.000000000000000d0)
@end(code)
")

