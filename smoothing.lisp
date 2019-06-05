;;;; smoothing.lisp

(in-package #:math)

(export 'gauss-smoothing)
(defun gauss-smoothing (d)
  "
Тестирование:
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (gauss-smoothing d)))
"
  (exp (* -1 d d)))

(export 'exp-smoothing)
(defun exp-smoothing (d)
  "
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (exp-smoothing d)))
"
  (exp (* -1 d)))

(export 'cauchy-smoothing)
(defun cauchy-smoothing (d)
  "
Тестирование:
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (cauchy-smoothing d)))
"
  (/ 1 (+ 1 (* d d))))

(export 'hann-smoothing)
(defun hann-smoothing (d)
  "
Тестирование:
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (hann-smoothing d)))
"
  (if (< d 0.5)
      (* 1/2 ( + 1 ( cos (* 2 pi d))))
      0))
