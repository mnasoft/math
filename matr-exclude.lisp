;;;; matr-exclude.lisp

(in-package #:math)

(export 'matr-osr-lambda)
(defun matr-osr-lambda (vv ff ex_pts)
  "Пример использования:
  (matr-osr-func '(xx yy) 
		 '((xx xx) (xx) (1.0) (yy)) 
		 '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0))
		 'coool-func)
;
=>(\"Matr\" 1 3 ((0 . 1.0d0) (1 . 0.0d0) (2 . 0.0d0)))
"
  (let ((kk (cons '+ (mapcar #'(lambda(el1 el2) (cons '* (cons el1 el2)))
			     (matr-to-point 
			      (solve-linear-system-gauss
			       (matr-mnk vv ff ex_pts)))
			     ff)))
	(rez nil))
    (setf rez (list 'lambda (reverse (cdr(reverse vv))) kk))
    rez))
