;;;; smoothing.lisp
;;;; ./src/smooth/smooth.lisp

(defpackage #:math/smooth
  (:use #:cl)
  (:export gauss-smoothing
	   exp-smoothing
	   cauchy-smoothing
	   hann-smoothing)
  (:export weight-func-list
	   weight-func-p))

(in-package :math/smooth)

(defun gauss-smoothing (d)
  (exp (* -1 d d)))

(defun exp-smoothing (d)
  (exp (* -1 d)))

(defun cauchy-smoothing (d)
  (/ 1 (+ 1 (* d d))))

(defun hann-smoothing (d)
  (if (< d 1) (* 1/2 ( + 1 ( cos (* pi d)))) 0))

(defun weight-func-list ()
  (list #'gauss-smoothing
	#'exp-smoothing
	#'cauchy-smoothing
	#'hann-smoothing))

(defun weight-func-p (func)
  (if (member func (weight-func-list)) t nil))
