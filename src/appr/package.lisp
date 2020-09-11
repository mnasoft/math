;;;; ./appr/package.lisp

(defpackage #:math/appr 
  (:use #:cl #:math/arr-matr)
  (:export
   refine-smoothing-by-points
   refine-smoothing-q-q-q-q-q
   smooth-by-points
   appr-bilinear-x2
   *apr-func-2-5*
   *apr-func-2-6*
   appr-linear-a1d-func
   make-approximation-defun
         
   appr-linear-x1
   *apr-func-1-2*
   *apr-func-1-5*
   approximate
   appr-bilinear-a2d-func
   appr-bilinear-x1
   make-linear-interpolation
   make-appr-linear *apr-args-1*
   appr-table *apr-func-2-7*
   make-approximation-lambda
   *apr-func-2-8* *apr-func-2-4*
   *apr-func-2-9*
   averaging-function-defun
   <appr-linear>
   *apr-args-2*
   *apr-func-1-3*
   averaging-function-lambda
   <appr-bilinear>
   *apr-func-1-4*
   make-least-squares-matrix
   )) 

