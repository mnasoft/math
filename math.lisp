;;;; math.lisp

(in-package #:math)

;;; "math" goes here. Hacks and glory await!

(defun square(x)
  "Возвращает квадрат значения"
  (* x x))

(defun averange-value (x)
  "Возврвщает среднее значение для списка величин;
Пример использования:
;;;; (averange-value '(1.1 1.0 0.9 1.2 0.8))
;;;; => 1.0"
  (/ (apply #'+ x) (length x)))

(defun max-value (x)
  (apply #'max x))

(defun min-value (x)
  (apply #'min x))

(defun dispersion(x)
  "
;;;; (dispersion '(1.1 1.0 0.9 1.2 0.8))
;;;; => 0.025000006"
  (let* ((x-sr (averange-value x))
	 (n-1 (1- (length x)))
	 (summ (apply #'+ (mapcar #'(lambda (el) (square (- el x-sr))) x))))
    (/ summ n-1)))

(defun standard-deviation (x)
  "(standard-deviation '(1.1 1.0 0.9 1.2 0.8))
"
  (sqrt (dispersion x)))

(defun variation-coefficient (x)
  "Коэффициент вариации
(variation-coefficient '(1.1 1.0 0.9 1.2 0.8))
"
  (/ (standard-deviation x)
     (SR_ARIFM X)))

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

(defun depth-sphere-along-cone (r alpha)
  "Заглубление сферы радиуса R в конуc 
с углом при вершине равным alpha  
от линии пересечения конуса с цилиндром."
  (let ((betta (- pi (/ alpha 2))))
	(- r (* r (tan (/ betta  2))))))
