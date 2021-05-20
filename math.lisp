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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (asdf:system-description (asdf:find-system :math))
      (uiop:read-file-form 
       (make-pathname
        :directory (pathname-directory
                    (asdf:system-definition-pathname :math))
        :name "description")))

