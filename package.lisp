;;;; package.lisp

(defpackage #:math	       
  (:use #:cl #:cl-utilities)
  (:export appr_table)
  (:export matr-name
	   matr-rows
	   matr-cols
	   ;; matr-elements
	   matr-ij    
	   matr-set_ij  
	   matr-set-row 
	   matr-get-row 
	   matr-set-col 
	   matr-get-col 
	   matr-new     
	   ;; matr-eval
	   matr-mult    
	   matr-to-point
	   point-to-matr
	   matr-copy
	   matr-to-string
	   matr-print
	   matr-mnk
	   matr-triang
	   matr-las-gauss
	   matr-osr-func
	   matr-las-rotation
	   ;; statistics
	   ;; averange
	   averange-value
	   averange-not-nil-value
	   ;; exclude
	   exclude-nil-from-list
	   max-value
   	   min-value
	   dispersion
	   standard-deviation
	   variation-coefficient
	   clean-flagrant-error 
	   make-random-value-list
	   clean-min-flagrant-error
	   clean-max-flagrant-error
	   ;;
	   matr-rotation
	   ;; list-matr
	   list-matr-transpose
	   list-matr-union
	   list-matr-rows
	   list-matr-cols
	   list-matr-row
	   list-matr-col
	   list-matr-averange-value
	   list-matr-averange-not-nil-value
	   list-matr-averange-row-value
	   list-matr-averange-row-not-nil-value
	   list-matr-averange-col-value
	   list-matr-averange-col-not-nil-value
	   list-matr-print
	   list-vector-print
	   list-matr-append-row
	   list-matr-append-col
	   list-matr-prepend-row
	   list-matr-prepend-col
;;;;
	   ))

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

