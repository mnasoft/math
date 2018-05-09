;;;; statistics.lisp

(in-package #:math)

;;; "math" goes here. Hacks and glory await!

(defun square(x)
  "Площадь"
  (* x x))

(defun averange (&rest x)
  "Возврвщает среднее значение для списка величин;
Пример использования:
;;;; (averange-value '(1.1 1.0 0.9 1.2 0.8))
;;;; => 1.0"
  (/ (apply #'+ x) (length x)))

(export 'averange)

(defun averange-value (x)
  "Возврвщает среднее значение для списка величин;
Пример использования:
;;;; (averange-value '(1.1 1.0 0.9 1.2 0.8))
;;;; => 1.0"
  (/ (apply #'+ x) (length x)))

(defun averange-not-nil-value (x)
  "Возврвщает среднее значение для списка величин ;
Пример использования:
;;;; (averange-not-nil-value '(1.1 1.0 nil 0.9 nil 1.2 nil 0.8))
;;;; => 1.0"
  (averange-value (exclude-nil-from-list x)))

(defun exclude-nil-from-list (lst)
  "Иключает из списка nil-элементы"
  (let ((res nil))
    (dolist (el lst (reverse res) )
      (when el (push el res )))))

(defun min-value (x)
  "Возврвщает минимальное значение для списка величин;
Пример использования:
;;;; (min-value '(1.1 1.0 0.9 1.2 0.8))
;;;; => 0.8"
  (apply #'min x))

(defun max-value (x)
  "Возврвщает максимальное значение для списка величин;
Пример использования:
;;;; (max-value '(1.1 1.0 0.9 1.2 0.8))
;;;; => 1.2"
  (apply #'max x))

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

(defun grubbs (n &optional (q 0.05))
  "Вычисляет значение критерия Граббса (п. 6.1 см. ГОСТ Р 8.736-2011)"
  (assert (<= 3 n 40) nil    "Количество значений должно быть между 3 и 40")
  (assert (>= q 0.01) nil "Уровень значимости q должен быть больше 0.01")
  (let ((arg-num (if (>= q 0.05) #'third #'second)))
    (funcall arg-num (assoc n *G-t*))))

(defun grubbs-1 (x)
    "Критерий Граббса для максимального значения"
  (let ((rez (sort (copy-list x) #'> )))
    (/ (- (first rez) (averange-value rez))
    (standard-deviation rez))))

(defun grubbs-2 (x)
  "Критерий Граббса для минимального значения"
  (let ((rez (sort (copy-list x) #'< )))
    (/ (- (averange-value rez) (first rez))
       (standard-deviation rez))))

(defun remove-last(x)
  "Удаляет из списка последний элемент"
  (reverse (cdr (reverse x))))

(defun remove-first(x)
  "Удаляет из списка первый элемент"
  (cdr x))

(defun clean-flagrant-error (x)
  "Удаляет из статистики грубые промахи"
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

(defun make-random-value-list ( mid-value  &key (std-deviation 1.0) (n 40) (top-level 1000000))
  "Создает список случайных величин
mid-value     - среднее значение;
std-deviation - стандартное отклонение;
n             - количество точек;
top-level     - дискретизация точек"
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

(defun clean-min-flagrant-error (x)
  "Удаляет из статистики грубые промахи"
  (do* ((lst (sort (copy-list x) #'<))
	(n (length lst) (1- n))
	(gr-2 (grubbs-2 lst) (grubbs-2 lst))
	(exiting nil))
       (exiting lst)
    (cond
      ((> gr-2 (grubbs n))
       (setf lst (remove-first lst)))
      (t (setf exiting t)))))

(defun clean-max-flagrant-error (x)
  "Удаляет из статистики грубые промахи"
  (do* ((lst (sort (copy-list x) #'<))
	(n (length lst) (1- n))
	(gr-1 (grubbs-1 lst) (grubbs-1 lst))
	(exiting nil))
       (exiting lst)
    (cond
      ((> gr-1 (grubbs n))
       (setf lst  (remove-last lst)))
      (t (setf exiting t)))))
