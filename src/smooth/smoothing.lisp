;;;; smoothing.lisp

(defpackage #:math/smooth
  (:use #:cl)
  (:export gauss-smoothing
	   exp-smoothing
	   cauchy-smoothing
	   hann-smoothing)
  (:export weight-func-list
	   weight-func-p))

(in-package :math/smooth)

(export 'gauss-smoothing)

(defun gauss-smoothing (d)
  "@b(Описание:) функция @b(gauss-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (gauss-smoothing d)))
@end(code)
"
  (exp (* -1 d d)))

(export 'exp-smoothing)

(defun exp-smoothing (d)
  "@b(Описание:) функция @b(exp-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (exp-smoothing d)))
@end(code)
"
  (exp (* -1 d)))

(export 'cauchy-smoothing)

(defun cauchy-smoothing (d)
  "@b(Описание:) функция @b(cauchy-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (cauchy-smoothing d)))
@end(code)
"
  (/ 1 (+ 1 (* d d))))

(export 'hann-smoothing)

(defun hann-smoothing (d)
  "@b(Описание:) функция @b(hann-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
  (loop :for d :from 0 :to 4 :by 0.1 :do
     (format t \"~{~5F~^ ~}~%\" 
             (list d 
               (gauss-smoothing  d)
               (exp-smoothing    d)
               (cauchy-smoothing d)
               (hann-smoothing   d))))
@end(code)
"
  (if (< d 1) (* 1/2 ( + 1 ( cos (* pi d)))) 0))

(export 'weight-func-list)

(defun weight-func-list ()
  (list #'gauss-smoothing
	#'exp-smoothing
	#'cauchy-smoothing
	#'hann-smoothing))

(export 'weight-func-p)

(defun weight-func-p (func)
  (if (member func (weight-func-list)) t nil))
