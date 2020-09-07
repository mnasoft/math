;;;; math.lisp

(defpackage #:math/core
  (:use #:cl)
  (:export summ-distance 
	   distance               
	   exclude-nil-from-list  
	   depth-sphere-along-cone
	   distance-relative
	   square))

(in-package :math/core)

(export 'square)
 
(defun square (x)
"@b(Описание:) функция @b(square) возвращает квадрат значения.

 @b(Переменые:)
@begin(list)
 @item(x - число.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (square 5) => 25 
 (square -4) => 16 
@end(code)
"
  (* x x))

(export 'exclude-nil-from-list )

(defun exclude-nil-from-list (lst)
"@b(Описание:) функция @b(exclude-nil-from-list) возвращает список в котором нет nil-элементов (они исключаются).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (exclude-nil-from-list '(1.1 1.0 nil 0.9 nil 1.2 nil 0.8)) 
 => (1.1 1.0 0.9 1.2 0.8)
@end(code)
"
  (let ((res nil))
    (dolist (el lst (reverse res) )
      (when el (push el res )))))

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

