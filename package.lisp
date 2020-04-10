;;;; package.lisp

(defpackage #:math)

(defpackage #:math 
  (:use #:cl #:cl-utilities)
;;;; approximation.lisp
  (:intern math::appr_table)

;;;; statistics.lisp  
  (:export
   math::square
   ;; averange
   math::averange
   math::averange-value
   math::averange-not-nil-value
   ;; exclude
   math::exclude-nil-from-list
   math::max-value
   math::min-value
   math::dispersion
   math::standard-deviation
   math::variation-coefficient
   math::clean-flagrant-error 
   math::make-random-value-list
   math::clean-min-flagrant-error
   math::clean-max-flagrant-error
;;;; las-rotation.lisp
   math::matr-rotation
;;;; list-matr.lisp
   math::list-matr-transpose
   math::list-matr-union
   math::list-matr-make
   math::list-matr-rows
   math::list-matr-cols
   math::list-matr-row
   math::list-matr-col
   math::list-matr-averange-value
   math::list-matr-averange-not-nil-value
   math::list-matr-averange-row-value
   math::list-matr-averange-row-not-nil-value
   math::list-matr-max-row-not-nil-value 
   math::list-matr-averange-col-value
   math::list-matr-averange-col-not-nil-value
   math::list-matr-max-col-not-nil-value 
   math::list-matr-print
   math::list-vector-print
   math::list-matr-append-row
   math::list-matr-append-col
   math::list-matr-prepend-row
   math::list-matr-prepend-col
   )
  
;;;; x-o.lisp
  (:export
   math::play
   math::depth-sphere-along-cone
   )
  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
