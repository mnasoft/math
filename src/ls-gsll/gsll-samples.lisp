(defparameter *m* '((1d0 1d0 1d0  1d0)
		    (1d0 2d0 3d0  4d0)
		    (1d0 4d0 9d0  16d0)
		    (1d0 8d0 27d0 64d0)))

(defparameter *v* '(10d0 30d0 100d0 354d0))

(let ((matrix (grid:make-foreign-array 'double-float :initial-contents *m*))
      (vec (grid:make-foreign-array 'double-float :initial-contents *v*)))
  (multiple-value-bind (matrix perm) (gsll:lu-decomposition matrix) (gsll:lu-solve matrix vec perm)))

(grid:copy-to (gsll:lu-solve matrix vec perm) grid:*default-grid-type*)

(MULTIPLE-VALUE-BIND (matrix PERM) (GSLL:LU-DECOMPOSITION *m*)
  (LET ((GSLL::X (GSLL:LU-SOLVE matrix *v* PERM)))
    (GRID:COPY-TO
     (GSLL:PERMUTE-INVERSE
      PERM
      (GSLL:MATRIX-PRODUCT-TRIANGULAR
       matrix
       (GSLL:MATRIX-PRODUCT-TRIANGULAR matrix GSLL::X 1 :UPPER :NOTRANS :NONUNIT) 1 :LOWER :NOTRANS :UNIT)))))





(GSLL:LU-DECOMPOSITION (grid:make-foreign-array 'double-float :initial-contents *m*))


(let ((matrix (grid:make-foreign-array 'double-float
				       :initial-contents '((-34.5d0   8.24d0    3.29d0 -8.93d0)
							   ( 34.12d0  -6.15d0  49.27d0 -13.49d0)
							   ( 32.5d0   42.73d0 -17.24d0  43.31d0)
							   (-16.12d0 -8.25d0   21.44d0 -49.08d0))))
      
      (VEC (GRID:MAKE-FOREIGN-ARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS '(-39.66d0 -49.46d0 19.68d0 -5.55d0))))
  (MULTIPLE-VALUE-BIND (MATRIX perm)
      (GSLL:LU-DECOMPOSITION MATRIX)
    (LET ((x (GSLL:LU-SOLVE MATRIX VEC perm)))
      (GRID:COPY-TO
       (GSLL:PERMUTE-INVERSE
	perm
	(GSLL:MATRIX-PRODUCT-TRIANGULAR MATRIX
					(GSLL:MATRIX-PRODUCT-TRIANGULAR MATRIX x 1 :UPPER :NOTRANS :NONUNIT)
					1 :LOWER :NOTRANS :UNIT))))))

  
(let ((matrix
	(grid:make-foreign-array '(complex double-float) :initial-contents
				 '((#c(-34.5d0   8.24d0)  #c( 3.29d0  -8.93d0) #c( 34.12d0  -6.15d0) #c(49.27d0  -13.49d0))
				   (#c( 34.12d0 -6.15d0) #c( 49.27d0 -13.49d0) #c( 32.5d0   42.73d0) #c(-17.24d0  43.31d0))
				   (#c( 32.5d0  42.73d0) #c(-17.24d0  43.31d0) #c(-16.12d0  -8.25d0) #c( 21.44d0 -49.08d0))
				   (#c(-16.12d0 -8.25d0) #c( 21.44d0 -49.08d0) #c(-39.66d0 -49.46d0) #c( 19.68d0  -5.55d0)))))
      (vec
	(grid:make-foreign-array
	 '(complex double-float)
	 :initial-contents
	 '(#c(-39.66d0 -49.46d0) #c( 19.68d0 -5.55d0) #c( -8.82d0 25.37d0) #c( -30.58d0 31.67d0)))))
  (multiple-value-bind (matrix perm)
      (gsll:lu-decomposition matrix)
    (let ((x (gsll:lu-solve matrix vec perm)))
      (grid:copy-to
       (gsll:permute-inverse
	perm
	(gsll:matrix-product-triangular matrix
					(gsll:matrix-product-triangular matrix x 1 :upper :notrans :nonunit)
					1 :lower :notrans :unit))))))

