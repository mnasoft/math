;;;; approximation-test.lisp

(in-package #:math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Тестирование
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *apr-1* (make-instance 'appr-linear :x1 (vector 0 1)  :a1d (vector 0 1 )))

(mapcar
 #'(lambda (el)
     (list (* (round el 0.1) 0.1) (* (round (approximate el *apr-1*) 0.01) 0.01)))
 (loop :for i :from -3 :to 3 :by 1/10 :collect i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Тестирование
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *test-data-01*
    '((117.0 120.1 118.6 112.5 115.0)
      (115.0 125.2 119.7 122.4 112.0)
      (113.0 135.3 129.8 132.3 117.0)
      (112.0 122.4 124.9 122.2 112.0)
      (111.0 115.5 119.5 102.1 102.0)))
  (defparameter *test-arr-data-01*
    (make-array '(5 5) :initial-contents *test-data-01*)))

(defparameter *apr-2* (make-instance 'appr-bilinear :a2d *test-arr-data-01* :x1 (vector 2 1 0 -1 -2) :x2 (vector -2 -1 0 1 2)))

(loop :for h :from -25 :to 25 :collect
     (loop :for x :from -25 :to 25 :collect
	  (approximate (vector (/ h 10.0) (/ x 10.0)) *apr-2*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gnuplot-2dlist (apr-class s)
  (format s "# x y z")
  (loop :for h :from -25 :to 25 :do
       (progn
	 (loop :for x :from -25 :to 25 :do
	      (let ((x1 (/ h 10.0))
		    (x2  (/ x 10.0)))
		(format s "~% ~F ~F ~F" x1 x2 (approximate (vector x1 x2) apr-class))))
	 (format s "~%"))))

(with-open-file
    (os "d:/PRG/msys32/home/namatv/gp3.data" :direction :output :if-exists :supersede)
  (gnuplot-2dlist *apr-2* os))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun d-i (x xi) (sqrt (* (- x xi) (- x xi))))

(defun gauss-w (d) (expt (* -1 d d)))

(let ((w-summ 0.0)
      (w-z-summ 0.0))
  (loop :for i :from 0 :below  (array-dimension vx 0) :do
       (progn
	 (setf w-summ   () )
	 (setf w-z-summ )
	 )
       ))


