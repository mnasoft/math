;;;; approximation.lisp

(in-package :math)

(defparameter *arr-2xN* (make-array '(5 2) :initial-contents '((-2.0 0.91553) (-1.0 1.15765) (0.0 1.68105) (1.0 1.15759) (2.0 0.9213))))

(defparameter *a-rez* (math:gauss-1-approximation-array *arr-2xN* :dx0 1.0 :delta 0.000001 :iterations 100))

(defun g-1-0 (x) (spline-1d x 1.0 *a-rez*))

(gnuplot-data-to-file (make-2d-list-by-func 'g-1-0 :x-from -25/10 :x-to 25/10 :steps 100) "~/apr.data")

(defmethod gnuplot-data-to-file ((ar array) f-name)
  (with-open-file (os f-name :direction :output :if-exists :supersede )
    (loop :for i :from 0 :below (array-dimension ar 0) :do
	 (loop :for j :from 0 :below (array-dimension ar 1) :do
	      (format os "~F " (aref ar i j)))
	 (format os "~%" ))))

(gnuplot-data-to-file *arr-2xn* "~/pts.data")
	 
(make-2d-list-by-func 'g-1-0 :x-from -25/10 :x-to 25/10 :steps 100)

(defun g-1-0 (x) (spline-1d x 1.0 *arr-2xN*))
(gnuplot-list-to-file (make-2d-list-by-func 'g-1-0 :x-from -25/10 :x-to 25/10 :steps 100)  "~/gauss-1.0.data")


(let ((dx 1.5))
  (gnuplot-list-to-file (loop :for x :from 0 :to 4 :by 1/50 :collect
			     (list x (spline-1d x dx *arr-2xN*)))
			"d:/PRG/msys/home/namatv/test.data")
  (gnuplot-list-to-file (loop :for x :from 0 :to 4 :by 1/50 :collect
			     (list x (spline-1d x dx *arr-2xN* :w-func #'exp-smoothing)))
			"d:/PRG/msys/home/namatv/test1.data")

  (gnuplot-list-to-file (loop :for x :from 0 :to 4 :by 1/50 :collect
			     (list x (spline-1d x dx *arr-2xN* :w-func #'cauchy-smoothing)))
			"d:/PRG/msys/home/namatv/test2.data")

  (gnuplot-list-to-file (loop :for x :from 0 :to 4 :by 1/50 :collect
			     (list x (spline-1d x dx *arr-2xN* :w-func #'hann-smoothing)))
			"d:/PRG/msys/home/namatv/test3.data")
  
  (gnuplot-list-to-file (loop :for x :from 0 :to 4 :by 1/50 :collect
			     (list x (spline-1d x dx *arr-2xN* :w-func #'mann-w-1d)))
			"d:/PRG/msys/home/namatv/test4.data")

  )

gnuplot
plot "test.data" with lines, "test1.data" with lines, "test2.data" with lines, "test3.data" with lines, "test4.data" with lines;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(math/gnuplot:gnuplot-data-plot
 "y-x2" 
 (let* ((nod-pts #(-2.0 -1.0 -0.5 0.0 0.5  1.0 2.0))
	(nod-rez #( 4.0  1.0 0.25 0.0 0.25 1.0 4.0))
	(base-dists-1_5 1.5 )
	(base-dists-1_0 1.0 )
	(base-dists-0_6 0.6 )
	(base-dists-0_4 0.4 )
	(func (make-refine-smoothing nod-pts nod-rez base-dists-1_5)))
   (loop :for i :from -2 :to 2 :by 1/10
	 :collect (list (* 1.0 i)
			(* 1.0 i i)
			(funcall func (* 1.0 i))
			(* 100 (1- (/ (* 1.0 i i)
				      (funcall func (* 1.0 i)))))))))
