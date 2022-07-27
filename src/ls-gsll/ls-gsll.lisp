;;;; ./src/ls-solve/ls-solve.lisp

(defpackage #:math/ls-gsll
  (:use #:cl )
  (:export lu-solve
           lu-solve-extmatr)
  (:documentation
   "@b(Описание:) пакет @b(math/ls-gsll) пределяет функции для
 решения СЛАУ методом LU-разложения при помощи системσ GSLL."))

(in-package #:math/ls-gsll)

(defun lu-solve (matrix vector)
  (multiple-value-bind (upper permutation signum) (gsl:lu-decomposition (grid:copy matrix))
    (declare (ignore signum))
    (let ((initial-solution (gsl:lu-solve upper vector permutation t)))
      (gsl:lu-refine matrix upper permutation vector initial-solution))))

(defun lu-solve-extmatr (m-v &key (grid-type 'array) (element-type 'double-float))
  (let* ((m (math/list-matr:detach-last-col m-v))
         (v (math/list-matr:get-last-col    m-v))
         (matrix
           (grid:make-foreign-array element-type :initial-contents m))
         (vector
           (grid:make-foreign-array element-type :initial-contents v)))
    (grid:copy
     (lu-solve matrix vector) 
     :grid-type  grid-type
     :element-type element-type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

