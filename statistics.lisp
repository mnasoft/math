;;;; statistics.lisp

(in-package #:math)
(annot:enable-annot-syntax)

@export @annot.doc:doc "
Возвращает квадрат значения"
(defun square (x)
  (* x x))

@export @annot.doc:doc "
Возврвщает среднее значение для списка величин;

 @b(Пример использования:)
@begin[lang=lisp](code)
 (averange-value '(1.1 1.0 0.9 1.2 0.8)) => 1.0 
@end(code)
"
(defun averange (&rest x)
  (/ (apply #'+ x) (length x)))

@export
@annot.doc:doc
  "Возвращает среднее значение для списка величин;

 @b(Пример использования:)
@begin[lang=lisp](code)
(averange-value '(1.1 1.0 0.9 1.2 0.8)) => 1.0
@end(code)
"
(defun averange-value (x)
  (/ (apply #'+ x) (length x)))

@export
@annot.doc:doc
  "Возврвщает среднее значение для списка величин ;

 @b(Пример использования:)
@begin[lang=lisp](code)
 (averange-not-nil-value '(1.1 1.0 nil 0.9 nil 1.2 nil 0.8)) => 1.0 
@end(code)
"
(defun averange-not-nil-value (x)
  (averange-value (exclude-nil-from-list x)))

@export
@annot.doc:doc
"Иключает из списка nil-элементы

 @b(Пример использования:)
@begin[lang=lisp](code)
 (exclude-nil-from-list '(1.1 1.0 nil 0.9 nil 1.2 nil 0.8)) 1.0 
@end(code)
"
(defun exclude-nil-from-list (lst)
  (let ((res nil))
    (dolist (el lst (reverse res) )
      (when el (push el res )))))

@export
@annot.doc:doc
"@b(Описание:) функция min-value возвращает максимальное значение для списка величин

 @b(Пример использования:)
@begin[lang=lisp](code)
(min-value '(1.1 1.0 0.9 1.2 0.8)) => 0.8 
@end(code)
"
(defun min-value (x)
  (apply #'min x))

@export
@annot.doc:doc
  "Возвращает отклонение минимальной величины от среднего значения
для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (delta-min-value '(1.1 1.0 0.9 1.2 0.8)) -0.19999999 
@end(code)
"
(defun delta-min-value (x) (- (min-value x) (averange-value x)))

@export
@annot.doc:doc
"STUB"
(defun min-not-nil-value (x)
  (min-value (exclude-nil-from-list x)))

@export
@annot.doc:doc
  "@b(Описание:) функция max-value возвращает максимальное значение для списка величин

 @b(Пример использования:)
@begin[lang=lisp](code)
(max-value '(1.1 1.0 0.9 1.2 0.8)) => 1.2 
@end(code)
"
(defun max-value (x)
  (apply #'max x))

@export
@annot.doc:doc
"Возвращает отклонение максимальной величины от среднего значения
для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (delta-max-value '(1.1 1.0 0.9 1.2 0.8)) => 0.20000005
@end(code)
"  
(defun delta-max-value (x)
  (- (max-value x) (averange-value x)))

@export
@annot.doc:doc
"STUB"
(defun max-not-nil-value (x)
  (max-value (exclude-nil-from-list x)))

@export
@annot.doc:doc
"@b(Описание:) функция dispersion возвращает дисперсию для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dispersion '(1.1 1.0 0.9 1.2 0.8)) => 0.025000006
@end(code)
"
(defun dispersion (x)
  (let* ((x-sr (averange-value x))
	 (n-1 (1- (length x)))
	 (summ (apply #'+ (mapcar #'(lambda (el) (square (- el x-sr))) x))))
    (/ summ n-1)))

@export
@annot.doc:doc
"@b(Описание:) возвращает среднеквадратичное (стандартное) отклонение для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (standard-deviation '(1.1 1.0 0.9 1.2 0.8)) => 0.1581139
@end(code)
"
(defun standard-deviation (x) (sqrt (dispersion x)))

@export
@annot.doc:doc
"@b(Описание:) возвращает коэффициент вариации для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (variation-coefficient '(1.1 1.0 0.9 1.2 0.8))
@end(code)
"
(defun variation-coefficient (x)
  (/ (standard-deviation x)
     (averange-value X)))

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

@export
@annot.doc:doc
"Вычисляет значение критерия Граббса (п. 6.1 см. ГОСТ Р 8.736-2011)"
(defun grubbs (n &optional (q 0.05))
  (assert (<= 3 n 40) nil    "Количество значений должно быть между 3 и 40")
  (assert (>= q 0.01) nil "Уровень значимости q должен быть больше 0.01")
  (let ((arg-num (if (>= q 0.05) #'third #'second)))
    (funcall arg-num (assoc n *G-t*))))

@export
@annot.doc:doc
"Критерий Граббса для максимального значения"
(defun grubbs-1 (x)
  (let ((rez (sort (copy-list x) #'> )))
    (/ (- (first rez) (averange-value rez))
       (standard-deviation rez))))

@export
@annot.doc:doc
"Критерий Граббса для максимального значения"
(defun grubbs-2 (x)
  (let ((rez (sort (copy-list x) #'< )))
    (/ (- (averange-value rez) (first rez))
       (standard-deviation rez))))

@export
@annot.doc:doc
"Удаляет из списка последний элемент"
(defun remove-last (x)
  (reverse (cdr (reverse x))))

@export
@annot.doc:doc
"Удаляет из списка первый элемент"
(defun remove-first (x) (cdr x))

@export
@annot.doc:doc
"Удаляет из статистики грубые промахи"
(defun clean-flagrant-error (x)
  (do* ((lst (sort (copy-list x) #'<))
	(n (length lst) (1- n))
	(gr-1 (grubbs-1 lst) (grubbs-1 lst))
	(gr-2 (grubbs-2 lst) (grubbs-2 lst))
	(exiting nil))
       (exiting lst)
    (cond
      ((> gr-1 (grubbs n))
       (setf lst  (remove-last lst)))
      ((> gr-2 (grubbs n))
       (setf lst (remove-first lst)))
      (t (setf exiting t)))
    ;;;;(break "~S ~S~%" exiting lst)
    ))

@export
@annot.doc:doc
"Создает список случайных величин:

 @b(Переменые:)
@begin(list)
 @item(mid-value     - среднее значение; )
 @item(std-deviation - стандартное отклонение;)
 @item(n             - количество точек; )
 @item(top-level     - дискретизация точек)
@end(list)
"
(defun make-random-value-list ( mid-value  &key (std-deviation 1.0) (n 40) (top-level 1000000))
  (let ((x nil))
    (dotimes (i n)
      (push (+ mid-value
	       (* std-deviation 3.4725
		  (/ (- (random top-level) (/ (- top-level 1) 2)) top-level )))
	    x))
    (values x
	    (averange-value x)
	    (standard-deviation x)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"Удаляет из статистики грубые промахи"
(defun clean-min-flagrant-error (x)
  (do* ((lst (sort (copy-list x) #'<))
	(n (length lst) (1- n))
	(gr-2 (grubbs-2 lst) (grubbs-2 lst))
	(exiting nil))
       (exiting lst)
    (cond
      ((> gr-2 (grubbs n))
       (setf lst (remove-first lst)))
      (t (setf exiting t)))))

@export
@annot.doc:doc
"Удаляет из статистики грубые промахи"
(defun clean-max-flagrant-error (x)
  (do* ((lst (sort (copy-list x) #'<))
	(n (length lst) (1- n))
	(gr-1 (grubbs-1 lst) (grubbs-1 lst))
	(exiting nil))
       (exiting lst)
    (cond
      ((> gr-1 (grubbs n))
       (setf lst  (remove-last lst)))
      (t (setf exiting t)))))
