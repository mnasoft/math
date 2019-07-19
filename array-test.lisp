;;;; array.lisp

(in-package #:math)

(defparameter *a* (make-array '(2 3) :initial-contents '((1 2 3)(4 5 6))))

(setf (col *a* 0) 101)
(setf (row *a* 0) 201)

(setf (col *a* 0) (vector 11  12))
(setf (row *a* 0) (vector 21  22 23))

(setf (col *a* 0) (list 11  12))
(setf (row *a* 1) (list 21  22 23))
