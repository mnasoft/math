;;;; test.lisp

(in-package #:math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;(require :temperature-fild)
(defparameter *h-r*
  (apply #'vector
   (reverse
    (temperature-fild:t-fild-termopara-hight-relative
     temperature-fild:*dn80*))))

(defparameter *x-r*
  (apply #'vector (list -2 -1  0  1  2)))

(defparameter *a* (make-array '(5 5) :initial-contents
			      '((117.0 120.5 118.5 112.5 115.0)
				(115.0 125.5 119.5 122.5 112.0)
				(113.0 135.5 129.5 132.5 117.0)
				(112.0 122.5 123.5 122.5 112.0)
				(111.0 115.5 119.5 102.5 102.0))))

(defparameter *v-ref* 
  (let* ((pts-vals (multiple-value-list (make-points-values *a* *h-r* *x-r*)))
	 (d-pts (vector 0.2 1.0))
	 (pts  (first pts-vals))
	 (vals (second  pts-vals))
	 (r-vals (refine-approximation-values pts vals d-pts :delta 0.01))
	 (ch-vals (make-array (list (length *h-r*) (length *x-r* )) :initial-element 0.0))
	 (line  nil)
	 (net-v nil))
    (loop :for i :from 0 :below (length *h-r*) :do
	 (loop :for j :from 0 :below (length *x-r*) :do
	      (setf (aref ch-vals i j) (approx-by-points  (vector (svref *h-r* i) (svref *x-r* j)) d-pts pts r-vals))))
        ch-vals
    (loop :for i :from 1 :downto 0 :by 1/100 :collect 
	 (loop :for j :from -25/10 :to 25/10 :by 50/1000 :collect
	      (list  (coerce  j 'float) (coerce  i 'float)
		    (approx-by-points  (vector  i j) d-pts pts r-vals))))))

;;;; (gnuplot-data-splot "~/splot.data" *v-ref*)

;;;; gnuplot splot 'splot.data' u 1:2:3 w l

;;;; splot "grid" u 1:2:3 w l
;;;; splot "matrix" nonuniform matrix u 1:2:3 w l
;;;; pos( k, origin, spacing ) = origin + k*spacing
;;;; splot "packedmatrix" matrix u (pos($1,0,1)):(pos($2,-1,1)):3 w l

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
