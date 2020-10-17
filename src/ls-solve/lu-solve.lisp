;;;; ls-solve/lu-solve.lisp

(in-package #:math-ls-solve)

;;; В данном файле определяются некоторые операции над матрицами,
;;; представленными 2d-list, (списком состоящим из списков)


(defun lu-solve (matrix vector)
  "@b(Описание:) функция| @b(lu-solve) возвращает корни решения СЛАУ 
(системы линейных алгебраических уравнений), 
используя LU-разложение матрицы @b(matrix).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((m (grid:make-foreign-array 'double-float :initial-contents '((1 2 3)  
								     (2 1 1)  
								     (3 0 1)))) 
       (v (grid:make-foreign-array 'double-float :initial-contents '(14 7 6))))
   (lu-solve m v)) => 
#m(1.000000000000000d0 2.000000000000000d0 3.000000000000000d0)

@end(code)
"
  (multiple-value-bind (upper permutation signum) (gsl:lu-decomposition (grid:copy matrix))
    (declare (ignore signum))
    (let ((initial-solution (gsl:lu-solve upper vector permutation t)))
      (gsl:lu-refine matrix upper permutation vector initial-solution))))

(defun lu-solve-extmatr (matrix-vector &key (grid-type 'array) (element-type 'double-float))
  ""
  (let ((matrix (grid:make-foreign-array
		 'double-float
		 :initial-contents
		 (math/list-matr:detach-last-col matrix-vector)))
	(vector (grid:make-foreign-array
		 'double-float
		 :initial-contents
		 (math/list-matr:get-last-col    matrix-vector))))
    (grid:copy
     (lu-solve matrix vector) 
     :grid-type  grid-type
     :element-type element-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

