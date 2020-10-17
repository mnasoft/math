;;;; /src/arr-matr/package.lisp

(defpackage #:math/arr-matr
  (:use #:cl #:math/core)
  (:export matr-new
	   <matrix>

	   math/core:mref
   
	   math/core:col
           math/core:row
  	   
	   math/core:cols
   	   math/core:rows

	   swap-cols
	   swap-cols*
	   swap-rows
	   swap-rows*

	   math/core:add
	   math/core:multiply
	   
	   math/core:main-diagonal
	   
	   *semi-equal-zero*
	   math/core:dimensions
	   initialize-instance
	   math/core:squarep
	   math/core:anti-diagonal
	   semi-equal
	   math/core:copy
	   matrix-data

	   convert-to-triangular
	   math/core:equivalent
	   matrix->2d-list

	   *semi-equal-relativ*
	   transpose 


	   matr-eval-*

	   ))
