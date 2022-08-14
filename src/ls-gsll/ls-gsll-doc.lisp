(in-package #:MATH/LS-GSLL)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



(make-doc
  (find-package 'MATH/LS-GSLL) t
  "@b(Описание:) пакет @b(math/ls-gsll) пределяет функции для
 решения СЛАУ методом LU-разложения при помощи системσ GSLL.")

(make-doc
  (macro-function 'MATH/LS-GSLL::MAKE-DOC) t
  NIL)

(make-doc
  #'MATH/LS-GSLL:SOLVE-X 'function
  "@b(Описание:) функция| @b(solve) возвращает корни решения СЛАУ
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
   (solve-x m))
=> #(1.0000000000000002d0 2.0000000000000004d0 2.9999999999999996d0)
@end(code)")

(make-doc
  #'MATH/LS-GSLL:SOLVE 'function
  "@b(Описание:) функция| @b(solve) возвращает корни решения СЛАУ 
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
   (solve m v))
=> #(1.0000000000000002d0 2.0000000000000004d0 2.9999999999999996d0)
@end(code)
")

(make-doc
  #'MATH/LS-GSLL::FIND-SLOT 'function
  NIL)
