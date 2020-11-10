;;;; statistics.lisp

(defpackage #:math/stat
  (:use #:cl #:math/core)
  (:export make-random-value-list
	   grubbs-min
	   average-not-nil-value
	   average-value
	   grubbs-max
	   delta-min-value average
	   dispersion min-value
	   max-not-nil-value
	   max-value
	   variation-coefficient
	   clean-max-flagrant-error
	   grubbs delta-max-value
	   min-not-nil-value
	   standard-deviation
	   clean-min-flagrant-error
	   clean-flagrant-error)
  (:export aver-max-min
	   aver-dmax-dmin))

(in-package :math/stat)

(export 'average )

(defun average (&rest x)
"@b(Описание:) функция @b(average) возврвщает среднее значение для перечня величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average 1.1 1.0 0.9 1.2 0.8) => 1.0 
@end(code)
"
  (assert (< 0 (length x)))
  (/ (apply #'+ x) (length x)))

(export 'average-value )

(defun average-value (x)
"@b(Описание:) функция @b(average-value) возвращает среднее значение для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
(average-value '(1.1 1.0 0.9 1.2 0.8)) => 1.0
@end(code)
"
  (apply #'average x))

;;;;  (assert (< 0 (length x))) (/ (apply #'+ x) (length x))

(export 'average-not-nil-value )

(defun average-not-nil-value (x)
  "@b(Описание:) функция @b(average-not-nil-value) возвращает среднее значение
для списка величин.

 @b(Переменые:)
@begin(list)
@item(x - список, содержащий числа или nil.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-not-nil-value '(1.1 1.0 nil 0.9 nil 1.2 nil 0.8)) => 1.0 
@end(code)
"
  (apply #'average (exclude-nil-from-list x)))

(export 'min-value )

(defun min-value (x)
"@b(Описание:) функция @b(min-value) возвращает максимальное значение для списка величин

 @b(Пример использования:)
@begin[lang=lisp](code)
(min-value '(1.1 1.0 0.9 1.2 0.8)) => 0.8 
@end(code)
"
  (apply #'min x))

(export 'delta-min-value )

(defun delta-min-value (x)
"@b(Описание:) функция @b(delta-min-value)
возвращает отклонение минимальной величины от среднего значения
для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (delta-min-value '(1.1 1.0 0.9 1.2 0.8)) -0.19999999 
@end(code)
"
  (- (min-value x) (apply #'average x)))

(export 'min-not-nil-value )

(defun min-not-nil-value (x)
"@b(Описание:) функция min-not-nil-value возвращает минимальное значение для списка величин.

 @b(Переменые:)
@begin(list)
 @item(x - список, содержащий числовые значения или nil.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (min-not-nil-value '(nil 20 nil 5 nil 10)) => 5 
@end(code)
"
  (min-value (exclude-nil-from-list x)))

(export 'max-value )

(defun max-value (x)
  "@b(Описание:) функция max-value возвращает максимальное значение для списка величин

 @b(Пример использования:)
@begin[lang=lisp](code)
(max-value '(1.1 1.0 0.9 1.2 0.8)) => 1.2 
@end(code)
"
  (apply #'max x))

(export 'delta-max-value )

(defun delta-max-value (x)
"Возвращает отклонение максимальной величины от среднего значения
для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (delta-max-value '(1.1 1.0 0.9 1.2 0.8)) => 0.20000005
@end(code)
"  
  (- (max-value x) (apply #'average x)))

(export 'max-not-nil-value )

(defun max-not-nil-value (x)
"@b(Описание:) функция max-not-nil-value возвращает максимальное значение для списка величин.

 @b(Переменые:)
@begin(list)
 @item(x - список, содержащий числовые значения или nil.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (max-not-nil-value '(nil 20 nil 5 nil 10)) => 20
@end(code)
"
  (max-value (exclude-nil-from-list x)))

(export 'dispersion )

(defun dispersion (x)
"@b(Описание:) функция dispersion возвращает дисперсию для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dispersion '(1.1 1.0 0.9 1.2 0.8)) => 0.025000006
@end(code)
"
  (let* ((x-sr (apply #'average x))
	 (n-1 (1- (length x)))
	 (summ (apply #'+ (mapcar #'(lambda (el) (square (- el x-sr))) x))))
    (/ summ n-1)))

(export 'standard-deviation )

(defun standard-deviation (x)
"@b(Описание:) функция standard-deviation возвращает среднеквадратичное 
(стандартное) отклонение для списка величин.

 @b(Переменые:)
@begin(list)
 @item(x - список, содержащий числовые значения.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (standard-deviation '(1.1 1.0 0.9 1.2 0.8)) => 0.1581139
@end(code)
"
  (sqrt (dispersion x)))

(export 'variation-coefficient )

(defun variation-coefficient (x)
"@b(Описание:) возвращает 
@link[uri=\"https://ru.wikipedia.org/wiki/Коэффициент_вариации\"](коэффициент вариации)
для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (variation-coefficient '(1.1 1.0 0.9 1.2 0.8))
@end(code)
"
  (/ (standard-deviation x)
     (apply #'average X)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *G-t*
  '((3 1.155 1.155)
    (4 1.496 1.481)
    (5 1.764 1.715)
    (6 1.973 1.887)
    (7 2.139 2.020)
    (8 2.274 2.126)
    (9 2.387 2.215)
    (10 2.482 2.290)
    (11 2.564 2.355)
    (12 2.636 2.412)
    (13 2.699 2.462)
    (14 2.755 2.507)
    (15 2.806 2.549)
    (16 2.852 2.585)
    (17 2.894 2.620)
    (18 2.932 2.651)
    (19 2.968 2.681)
    (20 3.001 2.709)
    (21 3.031 2.733)
    (22 3.060 2.758)
    (23 3.087 2.781)
    (24 3.112 2.802)
    (25 3.135 2.822)
    (26 3.157 2.841)
    (27 3.178 2.859)
    (28 3.199 2.876)
    (29 3.218 2.893)
    (30 3.236 2.908)
    (31 3.253 2.924)
    (32 3.270 2.938)
    (33 3.286 2.952)
    (34 3.301 2.965)
        (35 3.301 2.965) ;; Принято как для 34
    (36 3.330 2.991)
        (37 3.330 2.991) ;; Принято как для 36
    (38 3.356 3.014)
        (39 3.356 3.014) ;; Принято как для 38
    (40 3.381 3.036))
  "Критические значения Gt для критерия Граббса.
|----+-------------------------------------|
| n  | Одно наибольшее или одно наименьшее |
|    | значение при уровне значимости q    |
|----+------------------+------------------|
|    | сыше 1%          | свыше 5%         |
|----+-------------------------------------|
| 3  | 1.155            | 1.155            |
............................................
............................................
............................................
| 40 | 3.381            | 3.036            |
|----+------------------+------------------|
   см. ГОСТ Р 8.736-2011
"
  )

(export 'grubbs )

(defun grubbs (n &optional (q 0.05))
"@b(Описание:) функция grubbs вычисляет значение критерия Граббса (см. п. 6.1 см. ГОСТ Р 8.736-2011).

 @b(Переменые:)
@begin(list)
@item(n - количество повторяющихся измерений величины.)
@item(q - уровень значимости в доях.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((lst '(10.0 10.1 10.15 10.2 10.8 9.9 9.85 9.9 10.1)))
   (grubbs (length lst))) => 2.215 
@end(code)
"
  (assert (<= 3 n 40) nil    "Количество значений должно быть между 3 и 40")
  (assert (>= q 0.01) nil "Уровень значимости q должен быть больше 0.01")
  (let ((arg-num (if (>= q 0.05) #'third #'second)))
    (funcall arg-num (assoc n *G-t*))))

(export 'grubbs-max )

(defun grubbs-max (x)
"@b(Описание:) функция grubbs-max возврвщает значения критерия Граббса 
для максимального значения списка величин.

 @b(Переменые:)
@begin(list)
@item(x - список, содержащий числовые значения.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((lst '(10.0 10.1 10.15 10.2 10.8 9.9 9.85 9.9 10.1)))
   (grubbs-max lst)) => 2.4095862
@end(code)
"
  (/ (- (max-value x) (apply #'average x))
     (standard-deviation x)))

(export 'grubbs-min )

(defun grubbs-min (x)
"@b(Описание:) функция grubbs-min возврвщает значения критерия Граббса 
для минимального значения списка величин.

 @b(Переменые:)
@begin(list)
@item(x - список, содержащий числовые значения.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((lst '(10.0 10.1 10.15 10.2 10.8 9.9 9.7 9.9 10.1)))
   (grubbs-min lst)) => 1.2863455 
@end(code)
"
  (/ (- (apply #'average x) (min-value x) )
     (standard-deviation x)))

(export 'clean-flagrant-error )

(defun clean-flagrant-error (x)
"@b(Описание:) функция @b(clean-flagrant-error) удаляет из статистики грубые промахи.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((lst '(10.0 10.1 10.15 10.2 12.0 9.9 5.0 9.9 10.1)))
   (clean-flagrant-error lst)) (9.9 9.9 10.0 10.1 10.1 10.15 10.2)
@end(code)"
  (labels ((remove-last (x)
	     "Удаляет из списка последний элемент"
	     (reverse (cdr (reverse x))))
	   (remove-first (x)
	     "Удаляет из списка первый элемент"
	     (cdr x))
	   )
  (do* ((lst (sort (copy-list x) #'<))
	(n (length lst) (1- n))
	(gr-1 (grubbs-max lst) (grubbs-max lst))
	(gr-2 (grubbs-min lst) (grubbs-min lst))
	(exiting nil))
       (exiting lst)
    (cond
      ((> gr-1 (grubbs n)) (setf lst (remove-last  lst)))
      ((> gr-2 (grubbs n)) (setf lst (remove-first lst)))
      (t (setf exiting t))))))

(export 'make-random-value-list )

(defun make-random-value-list ( mid-value  &key (std-deviation 1.0) (n 40) (top-level 1000000))
"Создает список случайных величин:

 @b(Переменые:)
@begin(list)
 @item(mid-value     - среднее значение; )
 @item(std-deviation - стандартное отклонение;)
 @item(n             - количество точек; )
 @item(top-level     - дискретизация точек)
@end(list)
"
  (let ((x nil))
    (dotimes (i n)
      (push (+ mid-value
	       (* std-deviation 3.4725
		  (/ (- (random top-level) (/ (- top-level 1) 2)) top-level )))
	    x))
    (values x
	    (apply #'average x)
	    (standard-deviation x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-first (lst)
  (cdr lst))

(defun remove-last (lst)
  (reverse (cdr (reverse lst))))

(export 'clean-min-flagrant-error )

(defun clean-min-flagrant-error (x)
"Удаляет из статистики грубые промахи"
  (do* ((lst (sort (copy-list x) #'<))
	(n (length lst) (1- n))
	(gr-2 (grubbs-min lst) (grubbs-min lst))
	(exiting nil))
       (exiting lst)
    (cond
      ((> gr-2 (grubbs n))
       (setf lst (remove-first lst)))
      (t (setf exiting t)))))

(export 'clean-max-flagrant-error )

(defun clean-max-flagrant-error (x)
"Удаляет из статистики грубые промахи"
  (do* ((lst (sort (copy-list x) #'<))
	(n (length lst) (1- n))
	(gr-1 (grubbs-max lst) (grubbs-max lst))
	(exiting nil))
       (exiting lst)
    (cond
      ((> gr-1 (grubbs n))
       (setf lst  (remove-last lst)))
      (t (setf exiting t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(aver-max-min))

(defun aver-max-min (seq)
  "@b(Описание:) функция @b(aver-max-min) возвращает список,  состоящий из:
@begin(list)
 @item(среднего значения величины;)
 @item(максимального значения величины;)
 @item( минимального значения величины.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (aver-max-min '(7.3869333 9.938901 8.541331 10.828626 9.348187 11.323172))
 => (9.561192 11.323172 7.3869333)
@end(code)

"
  (let ((mid-v (math/stat:average-value seq))
	(max-v (math/stat:max-value seq))
	(min-v (math/stat:min-value seq)))
    (list mid-v max-v min-v)))

(export '(aver-dmax-dmin))

(defun aver-dmax-dmin (seq)
    "@b(Описание:) функция @b(aver-max-min) возвращает список, состоящий из:
@begin(list)
 @item(из среднего значения величины;)
 @item(отклонения максимального занчения в выборке от среднего;)
 @item(отклонения минимального занчения в выборке от среднего.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (aver-dmax-dmin '(7.3869333 9.938901 8.541331 10.828626 9.348187 11.323172))
 => (9.561192 1.76198 -2.1742582)
@end(code)
"
  (let ((mid-v (math/stat:average-value seq))
	(max-v (math/stat:max-value seq))
	(min-v (math/stat:min-value seq)))
    (list mid-v (- max-v mid-v) (- min-v mid-v))))
