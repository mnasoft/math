;;;; statistics.lisp

(in-package #:math)

;;; "math" goes here. Hacks and glory await!

(defun square(x)
  "Площадь"
  (* x x))

(defun averange-value (x)
  "Возврвщает среднее значение для списка величин;
Пример использования:
;;;; (averange-value '(1.1 1.0 0.9 1.2 0.8))
;;;; => 1.0"
  (/ (apply #'+ x) (length x)))

(defun dispersion(x)
  "Дисперсия случайной величины
Пример использования
;;;; (dispersion '(1.1 1.0 0.9 1.2 0.8))
;;;; => 0.025000006"
  (let* ((x-sr (averange-value x))
	 (n-1 (1- (length x)))
	 (summ (apply #'+ (mapcar #'(lambda (el) (square (- el x-sr))) x))))
    (/ summ n-1)))

(defun standard-deviation (x)
  "Среднеквадратичное отклонение (стандартное отклонение)
Пример использования:
(standard-deviation '(1.1 1.0 0.9 1.2 0.8))
"
  (sqrt (dispersion x)))

(defun variation-coefficient (x)
  "Коэффициент вариации для случайной величины
Пример использования:
;;;; (variation-coefficient '(1.1 1.0 0.9 1.2 0.8))
;;;; => 0.1581139
"
  (/ (standard-deviation x)
     (averange-value X)))
