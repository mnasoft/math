;;;; ./src/ls-solve/ls-solve.lisp

(defpackage :math/ls-gsll
  (:use #:cl )
  (:export solve
           solve-x)
  (:documentation
    "@b(Описание:) пакет @b(math/ls-gsll) пределяет функции для
 решения СЛАУ методом LU-разложения при помощи системσ GSLL."))

(in-package :math/ls-gsll)

(defun solve (matrix vector)
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
@end(code)"  
  (multiple-value-bind
        (upper permutation signum)
      (gsl:lu-decomposition
       (grid:copy matrix))
    (declare (ignore signum))
    (let* ((initial-solution (gsl:lu-solve upper vector permutation t))
           (rez (gsl:lu-refine matrix upper permutation vector initial-solution)))
      (apply #'vector
             (loop :for i :from 0 :below (grid:dim0 rez)
                   :collect (grid:gref rez i))))))

(defun solve-x (m-v)
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
@end(code)"  
  (let ((m (math/matr:detach-last-col m-v))
        (v (math/matr:get-last-col    m-v)))
    (solve (grid:make-foreign-array 'double-float :initial-contents m)
           (grid:make-foreign-array 'double-float :initial-contents v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

