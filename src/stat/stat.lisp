;;;; statistics.lisp

(defpackage #:math/stat
  (:use #:cl #:math/core)
  (:intern remove-first
           remove-last)
  (:export make-random-value-list
	   )
  (:export average
           dispersion
           standard-deviation
           variation-coefficient)
  (:export max-value
	   average-value
           min-value)
  (:export min-not-nil-value
           average-not-nil-value
           max-not-nil-value)
  (:export grubbs-max
           grubbs
           grubbs-min)
  (:export delta-max-value
	   delta-min-value)
  (:export clean-max-flagrant-error
           clean-min-flagrant-error
           clean-flagrant-error)
  (:export aver-max-min
	   aver-dmax-dmin))

(in-package :math/stat)

(defun remove-first (lst)
  (cdr lst))

(defun remove-last (lst)
  (reverse (cdr (reverse lst))))

(defun average (&rest x)
  (assert (< 0 (length x)))
  (/ (apply #'+ x) (length x)))

(defun average-value (x)
  (apply #'average x))

;;;;  (assert (< 0 (length x))) (/ (apply #'+ x) (length x))


(defun average-not-nil-value (x)
  (apply #'average (exclude-nil-from-list x)))

(defun min-value (x)
  (apply #'min x))

(defun delta-min-value (x)
  (- (min-value x) (apply #'average x)))

(defun min-not-nil-value (x)
  (min-value (exclude-nil-from-list x)))

(defun max-value (x)
  (apply #'max x))

(defun delta-max-value (x)
  (- (max-value x) (apply #'average x)))

(defun max-not-nil-value (x)
  (max-value (exclude-nil-from-list x)))

(defun dispersion (x)
  (let* ((x-sr (apply #'average x))
	 (n-1 (1- (length x)))
	 (summ (apply #'+ (mapcar #'(lambda (el) (square (- el x-sr))) x))))
    (/ summ n-1)))

(defun standard-deviation (x)
  (sqrt (dispersion x)))

(defun variation-coefficient (x)
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
    (40 3.381 3.036)))

(defun grubbs (n &optional (q 0.05))
  (assert (<= 3 n 40) nil    "Количество значений должно быть между 3 и 40")
  (assert (>= q 0.01) nil "Уровень значимости q должен быть больше 0.01")
  (let ((arg-num (if (>= q 0.05) #'third #'second)))
    (funcall arg-num (assoc n *G-t*))))

(defun grubbs-max (x)
  (/ (- (max-value x) (apply #'average x))
     (standard-deviation x)))

(defun grubbs-min (x)
  (/ (- (apply #'average x) (min-value x) )
     (standard-deviation x)))

(defun clean-flagrant-error (x)
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

(defun make-random-value-list ( mid-value  &key (std-deviation 1.0) (n 40) (top-level 1000000))
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



(defun clean-min-flagrant-error (x)
  (do* ((lst (sort (copy-list x) #'<))
	(n (length lst) (1- n))
	(gr-2 (grubbs-min lst) (grubbs-min lst))
	(exiting nil))
       (exiting lst)
    (cond
      ((> gr-2 (grubbs n))
       (setf lst (remove-first lst)))
      (t (setf exiting t)))))

(defun clean-max-flagrant-error (x)
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

(defun aver-max-min (seq)
  (let ((mid-v (math/stat:average-value seq))
	(max-v (math/stat:max-value seq))
	(min-v (math/stat:min-value seq)))
    (list mid-v max-v min-v)))

(defun aver-dmax-dmin (seq &optional (significant-digits +significant-digits+))
  (let* ((mid-v (math/stat:average-value seq))
	 (max-v (math/stat:max-value seq))
	 (min-v (math/stat:min-value seq))) 
    (list (round-to-significant-digits mid-v significant-digits)
	  (round-to-significant-digits (- max-v mid-v) significant-digits mid-v)
	  (round-to-significant-digits (- min-v mid-v) significant-digits mid-v))))
	  

