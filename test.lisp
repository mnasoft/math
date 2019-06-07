;;;; test.lisp

(in-package #:math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn 
  (require :temperature-fild)
  (defparameter *h-r*
    (apply #'vector
	   (reverse
	    (temperature-fild:t-fild-termopara-hight-relative
	     temperature-fild:*dn80*))))

  (defparameter *x-r*
    (apply #'vector (split-range -112/100 112/100 4)))

;;;; (split-range -140/100 140/100 10)
;;;; (split-range -112/100 112/100 4)

  (defparameter *a* (make-array '(5 5) :initial-contents
				'((117.0 120.5 118.5 112.5 115.0)
				  (115.0 125.5 119.5 122.5 112.0)
				  (113.0 135.5 129.5 132.5 117.0)
				  (112.0 122.5 123.5 122.5 112.0)
				  (111.0 115.5 119.5 102.5 102.0)))))

(gnuplot-data-splot "splot1" (make-t-fild-data *a* *h-r* *x-r* :hights '(1 0 1000) :ocr '(-140/100 140/100 1000)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
		     (approx-by-points  (vector  i j) d-pts pts r-vals))))
;;;;	(make-table (split-range 1 0 100) (split-range -25/10 25/10 100))    
    (table-apply-1
     (make-table (split-range 1 0 100) (split-range -25/10 25/10 100))
     #'approx-by-points d-pts pts r-vals)
    ))
