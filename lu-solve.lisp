;;;; lu-solve.lisp

;;; В данном файле определяются некоторые операции над матрицами,
;;; представленными 2d-list, (списком состоящим из списков)

(in-package #:math)

(defun lu-solve (matrix vector)
  "Solve the linear equation using LU with the supplied matrix and
   a right-hand side vector which is the reciprocal of one more than
   the index."
      (multiple-value-bind (upper permutation signum) (gsl:LU-decomposition (grid:copy matrix))
      (declare (ignore signum))
      (let ((initial-solution (gsl:LU-solve upper vector permutation T)))
	(gsl:LU-refine matrix upper permutation vector initial-solution))))

(defun lu-solve-extmatr (matrix-vector)
  ""
  (let ((matrix (grid:make-foreign-array
		 'double-float
		 :initial-contents
		 (list-matr-detach-last-col matrix-vector)))
	(vector (grid:make-foreign-array
		 'double-float
		 :initial-contents
		 (list-matr-get-last-col    matrix-vector))))
    (lu-solve matrix vector)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require :gsll)
