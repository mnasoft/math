;;;; ls-solve/temp.lisp

(in-package :math/ls-solve)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(make-instance 'grid:matrix :dimensions '(3 3) :element-type 'double-float  :initial-element 1.0d0)

(lu-solve-extmatr '((1 2 3 14) (2 1 1  7) (3 0 1  2)) :grid-type 'array :element-type 'double-float)

(math:equivalent)

(defparameter *a* (make-array '(2 3 ) :initial-element 10.0))

(defparameter *b* (make-array '(2 3) :initial-element 10.00001))




(deftype 2d-array (&optional type size1 size2 )
  `(array ,type (,size1 ,size2)))

(math:equivalent *a* *a*)

(math:equivalent *a* *b*)

(eql (and (arrayp *a*) (= 2 (array-rank *a*))) t )

(type-of *a*)   ; => (SIMPLE-ARRAY T (2 3))

(subtypep 

(eql (type-of *a*) (SIMPLE-ARRAY T (2 3)))
