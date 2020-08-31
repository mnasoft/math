;;;; math.lisp

(in-package #:math)

(defun e-value (n)
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
  (let ((rez 1)
	(nf 1))
    (dotimes (i n rez)
      (setf nf  (/ nf (1+ i))
	    rez (+ rez nf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'depth-sphere-along-cone)
(defun depth-sphere-along-cone (r alpha)
  "@b(Описание:) функция @b(depth-sphere-along-cone) возвращает 
заглубление сферы с радиусом R в конуc с углом при вершине 
равным alpha от линии пересечения конуса с цилиндром."
  (let ((betta (- pi (/ alpha 2))))
    (- r (* r (tan (/ betta  2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'distance)
(defgeneric distance (x1 x2)
  (:documentation "Возвращает расстояние между x1 и x2"))

(export 'distance)
(defmethod distance ((x1 number) (x2 number) )
  "@b(Описание:) 

@b(Пример использования:)
 (distance 1 0 ) => 1.0
 (distance 2 0 ) => 2.0
 (distance 2 1 ) => 1.0
"
  (let ((rez (- x1 x2)))
    (sqrt (* rez rez))))

(export 'distance)
(defmethod distance ((x1-lst cons) (x2-lst cons))
    "Пример использования:
 (distance '(1 1 1) '(0 0 0) ) 1.7320508
 (distance '(2 2 2) '(0 0 0) ) 3.4641016
 (distance '(2 1 2) '(0 0 0) ) 3.0
"
  (assert (= (length x1-lst) (length x1-lst)))
  (sqrt (apply #'+ (mapcar
		    #'(lambda (x1 x2)
			(let ((rez (- x1 x2)))
			  (* rez rez)))
		    x1-lst x2-lst))))

(export 'distance)
(defmethod distance ((x1 vector) (x2 vector))
  "Пример использования:
 (distance (vector 1 1 1) (vector 0 0 0)) 1.7320508
 (distance (vector 2 2 2) (vector 0 0 0)) 3.4641016
 (distance (vector 2 1 2) (vector 0 0 0)) 3.0
"
  (assert (= (length x1) (length x2)))
  (sqrt (apply #'+
	       (loop :for i :from 0 :below (array-dimension x1 0)
		     :collect
		     (let ((rez (- (svref x1 i) (svref x2 i)))) (* rez rez))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'distance-relative)
(defgeneric distance-relative (x xi dx)
  (:documentation "Возвращает относительную длину между x и xi, длина приведения dx"))

(export 'distance-relative)
(defmethod distance-relative ((x number) (xi number) (dx number))
    "Пример использования:
 (distance-relative 1 0 1) 1.0
 (distance-relative 2 0 1) 2.0
 (distance-relative 2 0 2) 1.0
"
  (let ((rez (/ (- x xi) dx)))
    (sqrt (* rez rez))))

(export 'distance-relative)
(defmethod distance-relative ((x-lst cons) (xi-lst cons) (dx-lst cons))
   "Пример использования:
 (distance-relative '(1 1 1) '(0 0 0) '(1 1 1)) 1.7320508
 (distance-relative '(2 2 2) '(0 0 0) '(1 1 1)) 3.4641016
 (distance-relative '(2 2 2) '(0 0 0) '(1 2 2)) 2.4494898
" 
  (sqrt (apply #'+ (mapcar
		    #'(lambda (x xi dx)
			(let ((rez (/ (- x xi) dx)))
			  (* rez rez)))
		    x-lst xi-lst dx-lst))))

(export 'distance-relative)
(defmethod distance-relative ((x vector) (xi vector) (dx vector))
  "Пример использования:
 (distance-relative (vector 1 1 1) (vector 0 0 0) (vector 1 1 1)) 1.7320508 
 (distance-relative (vector 2 2 2) (vector 0 0 0) (vector 1 1 1)) 3.4641016
 (distance-relative (vector 2 2 2) (vector 0 0 0) (vector 1 2 2)) 2.4494898
"
  (sqrt (apply #'+
  (loop :for i :from 0 :below (array-dimension x 0) :collect
       (let ((rez (/ (- (svref x i) (svref xi i)) (svref dx i)))) (* rez rez))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'summ-distance)
(defgeneric summ-distance (x1 x2)(:documentation "COOOL"))

(export 'summ-distance)
(defmethod summ-distance ((x1 vector) (x2 vector))
   "
Тестирование:
 (summ-distance (vector 1 2 3) (vector 2 3 4)) 3
"
  (assert (= (length x1) (length x2)))
  (apply #'+ (loop :for i :from 0 :below (length x1) :collect
		  (abs (- (svref x1 i) (svref x2 i))))))

(export 'summ-distance)
(defmethod summ-distance ((x1 cons) (x2 cons))
  "
Тестирование:
 (summ-distance '(1 2 3) '(2 3 4)) 3
"
  (assert (= (length x1) (length x2)))
  (apply #'+
	 (mapcar
	  #'(lambda (el1 el2)
	      (abs (- el1 el2)))
	  x1 x2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

