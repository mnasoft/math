;;;; package.lisp

(defpackage #:math 
  (:use #:cl #:math/core #:math/arr-matr)
;;;;  (:export  )
  )

(in-package :math )

(col cl-user::*m* 1)

(setf (mref cl-user::*m* 1 2) 10.5)

;;;; distance distance-relative mult-matr-vect depth-sphere-along-cone summ-distance

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
