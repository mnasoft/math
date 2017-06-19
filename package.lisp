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
	   ;;  matr-eval
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
	   ;;
	   averange-value
	   max-value
   	   min-value
	   dispersion
	   standard-deviation
	   variation-coefficient
	   ))

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
