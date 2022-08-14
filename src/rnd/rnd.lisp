;;;; ./src/rnd/rnd.lisp

(defpackage #:math/rnd
  (:use #:cl) 
  (:export make-1d-list 
           make-2d-list 
	   make-ls-system))

(in-package #:math/rnd)

(defun make-1d-list (size &optional (arg 15))
        (loop :for i :from 0 :below size
              :collect (random arg)))

(defun make-2d-list (size &optional (arg 15))
  (loop :for i :from 0 :below size
        :collect
        (loop :for j :from 0 :below size
              :collect (random arg))))

(defun make-ls-system (m vec)
  (assert (= (length m) (length vec)))
  (math/matr:append-col 
   (mapcar
    #'(lambda(el)
        (apply #'+ (mapcar #'(lambda(v a) (* v a)) vec el)))
    m)
   m))
