;;;; package.lisp

(defpackage #:math 
  (:use #:cl #:math/core #:math/arr-matr)
  (:export mult-matr-vect )
  (:export split-range
	   split-range-by-func)
  (:export row  col
	   rows cols))

(in-package :math)

;;;; (use-package (find-package :math/arr-matr) (find-package :math))
;;;; (use-package (find-package :math/core) (find-package :math))
