;;;; math.lisp

(in-package #:math)

(annot:enable-annot-syntax)

@annot.doc:doc
  "Возвращает приближенное значение числа e
Пример использования:
  (coerce (e-value      1) 'double-float) 2.0d0
  (coerce (e-value      2) 'double-float) 2.5d0
  (coerce (e-value      4) 'double-float) 2.7083333333333335d0
  (coerce (e-value      6) 'double-float) 2.7180555555555554d0
  (coerce (e-value      8) 'double-float) 2.71827876984127d0
  (coerce (e-value     10) 'double-float) 2.7182818011463845d0
  (coerce (e-value     15) 'double-float) 2.7182818284589945d0
  (coerce (e-value     16) 'double-float) 2.7182818284590424d0
  (coerce (e-value     17) 'double-float) 2.718281828459045d0
  (coerce (e-value  10000) 'double-float) 2.718281828459045d0
  (exp 1.0d0)                             2.718281828459045d0
" 
(defun e-value (n)
  (let ((rez 1)
	(nf 1))
    (dotimes (i n rez)
      (setf nf  (/ nf (1+ i))
	    rez (+ rez nf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"@b(Описание:) функция @b(depth-sphere-along-cone) возвращает 
заглубление сферы с радиусом R в конуc с углом при вершине 
равным alpha от линии пересечения конуса с цилиндром."
(defun depth-sphere-along-cone (r alpha)
  (let ((betta (- pi (/ alpha 2))))
    (- r (* r (tan (/ betta  2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defgeneric distance (x1 x2)
  (:documentation "Возвращает расстояние между x1 и x2"))

@export
@annot.doc:doc
"@b(Описание:) 

@b(Пример использования:)
 (distance 1 0 ) => 1.0
 (distance 2 0 ) => 2.0
 (distance 2 1 ) => 1.0
"
(defmethod distance ((x1 number) (x2 number) )
  (let ((rez (- x1 x2)))
    (sqrt (* rez rez))))

@export
@annot.doc:doc
  "Пример использования:
 (distance '(1 1 1) '(0 0 0) ) 1.7320508
 (distance '(2 2 2) '(0 0 0) ) 3.4641016
 (distance '(2 1 2) '(0 0 0) ) 3.0
"
(defmethod distance ((x1-lst cons) (x2-lst cons))
  (assert (= (length x1-lst) (length x1-lst)))
  (sqrt (apply #'+ (mapcar
		    #'(lambda (x1 x2)
			(let ((rez (- x1 x2)))
			  (* rez rez)))
		    x1-lst x2-lst))))

@export
@annot.doc:doc
  "Пример использования:
 (distance (vector 1 1 1) (vector 0 0 0)) 1.7320508
 (distance (vector 2 2 2) (vector 0 0 0)) 3.4641016
 (distance (vector 2 1 2) (vector 0 0 0)) 3.0
"
(defmethod distance ((x1 vector) (x2 vector))
  (assert (= (length x1) (length x2)))
  (sqrt (apply #'+
	       (loop :for i :from 0 :below (array-dimension x1 0) :collect
		    (let ((rez (- (svref x1 i) (svref x2 i)))) (* rez rez))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defgeneric distance-relative (x xi dx)
  (:documentation "Возвращает относительную длину между x и xi, длина приведения dx"))

@export
@annot.doc:doc
  "Пример использования:
 (distance-relative 1 0 1) 1.0
 (distance-relative 2 0 1) 2.0
 (distance-relative 2 0 2) 1.0
"
(defmethod distance-relative ((x number) (xi number) (dx number))
  (let ((rez (/ (- x xi) dx)))
    (sqrt (* rez rez))))

@export
@annot.doc:doc
 "Пример использования:
 (distance-relative '(1 1 1) '(0 0 0) '(1 1 1)) 1.7320508
 (distance-relative '(2 2 2) '(0 0 0) '(1 1 1)) 3.4641016
 (distance-relative '(2 2 2) '(0 0 0) '(1 2 2)) 2.4494898
" 
(defmethod distance-relative ((x-lst cons) (xi-lst cons) (dx-lst cons))

  (sqrt (apply #'+ (mapcar
		    #'(lambda (x xi dx)
			(let ((rez (/ (- x xi) dx)))
			  (* rez rez)))
		    x-lst xi-lst dx-lst))))
@export
@annot.doc:doc
"Пример использования:
 (distance-relative (vector 1 1 1) (vector 0 0 0) (vector 1 1 1)) 1.7320508 
 (distance-relative (vector 2 2 2) (vector 0 0 0) (vector 1 1 1)) 3.4641016
 (distance-relative (vector 2 2 2) (vector 0 0 0) (vector 1 2 2)) 2.4494898
"
(defmethod distance-relative ((x vector) (xi vector) (dx vector))
  (sqrt (apply #'+
  (loop :for i :from 0 :below (array-dimension x 0) :collect
       (let ((rez (/ (- (svref x i) (svref xi i)) (svref dx i)))) (* rez rez))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defgeneric summ-distance (x1 x2)(:documentation "COOOL"))

@export
@annot.doc:doc
 "
Тестирование:
 (summ-distance (vector 1 2 3) (vector 2 3 4)) 3
"
(defmethod summ-distance ((x1 vector) (x2 vector))
  (assert (= (length x1) (length x2)))
  (apply #'+ (loop :for i :from 0 :below (length x1) :collect
		  (abs (- (svref x1 i) (svref x2 i))))))

@export
@annot.doc:doc
  "
Тестирование:
 (summ-distance '(1 2 3) '(2 3 4)) 3
"
(defmethod summ-distance ((x1 cons) (x2 cons))
  (assert (= (length x1) (length x2)))
  (apply #'+
	 (mapcar
	  #'(lambda (el1 el2)
	      (abs (- el1 el2)))
	  x1 x2)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@annot.doc:doc
  "
Тестирование:
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (gauss-smoothing d)))
"
(defun gauss-smoothing (d)
  (exp (* -1 d d)))

@annot.doc:doc
  "
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (exp-smoothing d)))
"
(defun exp-smoothing (d)
  (exp (* -1 d)))

@annot.doc:doc
  "
Тестирование:
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (cauchy-smoothing d)))
"
(defun cauchy-smoothing (d)
  (/ 1 (+ 1 (* d d))))

@annot.doc:doc
  "
Тестирование:
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (hann-smoothing d)))
"
(defun hann-smoothing (d)
  (if (< d 0.5)
      (* 1/2 ( + 1 ( cos (* 2 pi d))))
      0))
