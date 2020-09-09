;;;; test.lisp

(in-package :math)

(math/gnuplot:split-range)
(math/gnuplot:split-range-by-func)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *v-ref* 
  (let* ((pts-vals (multiple-value-list (make-points-values *a* *h-r* *x-r*)))
	 (d-pts (vector 0.2 1.0))
	 (pts  (first pts-vals))
	 (vals (second  pts-vals))
	 (r-vals (refine-smoothing-by-points pts vals d-pts :delta 0.01))
	 (ch-vals (make-array (list (length *h-r*) (length *x-r* )) :initial-element 0.0))
	 (line  nil)
	 (net-v nil))
    (loop :for i :from 0 :below (length *h-r*) :do
	 (loop :for j :from 0 :below (length *x-r*) :do
	      (setf (aref ch-vals i j) (smooth-by-points  (vector (svref *h-r* i) (svref *x-r* j)) d-pts pts r-vals))))
    ch-vals
    (loop :for i :from 1 :downto 0 :by 1/100 :collect 
	 (loop :for j :from -25/10 :to 25/10 :by 50/1000 :collect
	      (list  (coerce  j 'float) (coerce  i 'float)
		     (smooth-by-points  (vector  i j) d-pts pts r-vals))))
;;;;	(make-table (split-range 1 0 100) (split-range -25/10 25/10 100))    
    (table-apply-1
     (make-table (split-range 1 0 100) (split-range -25/10 25/10 100))
     #'smooth-by-points d-pts pts r-vals)
    ))

