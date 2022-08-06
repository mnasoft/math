;;;; ./src/ls-solve/ls-solve.lisp

(defpackage #:math/ls-gsll
  (:use #:cl )
  (:export solve
           solve-x)
  (:documentation
   "@b(Описание:) пакет @b(math/ls-gsll) пределяет функции для
 решения СЛАУ методом LU-разложения при помощи системσ GSLL."))

(in-package #:math/ls-gsll)

(defun solve (matrix vector)
  (multiple-value-bind (upper permutation signum) (gsl:lu-decomposition (grid:copy matrix))
    (declare (ignore signum))
    (let* ((initial-solution (gsl:lu-solve upper vector permutation t))
           (rez (gsl:lu-refine matrix upper permutation vector initial-solution)))
      (apply #'vector
             (loop :for i :from 0 :below (grid:dim0 rez)
                   :collect (grid:gref rez i))))))

(defun solve-x (m-v)
  (let ((m (math/matr:detach-last-col m-v))
        (v (math/matr:get-last-col    m-v)))
    (solve (grid:make-foreign-array 'double-float :initial-contents m)
           (grid:make-foreign-array 'double-float :initial-contents v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

