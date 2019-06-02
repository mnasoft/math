;;;; approximation.lisp

(in-package #:math)


(defun d-i (x xi &key (dx 1.0))
  (let* ((rez (/ (- x xi) dx)))
    (sqrt (* rez rez))))

(defun gauss-w-1d (d) (exp (* -1 d d)))

(defun exp-w-1d (d) (exp (* -1 d)))

(defun cauchy-w-1d (d) (/ 1 (+ 1 (* d d))))

(defun hann-w-1d (d)
  (if (< -1 d 1)
      (* 1/2 ( - 1 ( cos (* 2 pi d))))
      0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exp-3-w-1d (d)
  "Моя отсебятина"
  (exp (* -1 d d d)))

(defun mann-w-1d (d)
  "Моя отсебятина"
  (if (< -1 d 1)
      ( cos (* 1/2 pi d))
      0))

(defun spline-1d (x dx arr-2xN &key (w-func #'gauss-w-1d))
  (let ((w-summ 0.0)
	(w-z-summ 0.0)
	(w 0.0)
	(z 0.0)
	)
    (loop :for i :from 0 :below  (array-dimension arr-2xN 0) :do
	 (progn
	   (setf w (funcall w-func (d-i x (aref arr-2xN i 0 ) :dx dx))
		 w-summ (+ w-summ w)
		 z (aref arr-2xN i 1)
		 w-z-summ (+ w-z-summ (* w z )))
;;	   (break "z=~A w=~A " z w w-z-summ w-summ )
	   ))
    (/ w-z-summ w-summ)))

(defparameter *arr-2xN* (make-array '(5 2) :initial-contents '((-2.0 0.91553) (-1.0 1.15765) (0.0 1.68105) (1.0 1.15759) (2.0 0.9213))))


(defun make-2d-list-by-func (func &key (x-from 0) (x-to 1) (step 100))
    (loop :for i :from x-from :to x-to :by (/ (- x-to x-from) step) :collect
	 (list (coerce i 'float) (coerce (funcall func i) 'float))))

(defun g-1-0 (x) (spline-1d x 1.0 *arr-2xN*))
(defun g-0-8 (x) (spline-1d x 0.8 *arr-2xN*))
(defun g-0-6 (x) (spline-1d x 0.6 *arr-2xN*))
(defun g-0-4 (x) (spline-1d x 0.4 *arr-2xN*))

(defun h-1-0 (x) (spline-1d x 1.0 *arr-2xN* :W-FUNC 'hann-w-1d)) 
(defun h-0-8 (x) (spline-1d x 0.8 *arr-2xN* :W-FUNC 'hann-w-1d))
(defun h-0-6 (x) (spline-1d x 0.6 *arr-2xN* :W-FUNC 'hann-w-1d))
(defun h-0-4 (x) (spline-1d x 0.4 *arr-2xN* :W-FUNC 'hann-w-1d))

(defun m-1-0 (x) (spline-1d x 1.0 *arr-2xN* :W-FUNC 'mann-w-1d)) 
(defun m-0-8 (x) (spline-1d x 0.8 *arr-2xN* :W-FUNC 'mann-w-1d))
(defun m-0-6 (x) (spline-1d x 0.6 *arr-2xN* :W-FUNC 'mann-w-1d))
(defun m-0-4 (x) (spline-1d x 0.4 *arr-2xN* :W-FUNC 'mann-w-1d))


(defun c-1-0 (x) (spline-1d x 1.0 *arr-2xN* :W-FUNC 'cauchy-w-1d)) 
(defun c-0-8 (x) (spline-1d x 0.8 *arr-2xN* :W-FUNC 'cauchy-w-1d))
(defun c-0-6 (x) (spline-1d x 0.6 *arr-2xN* :W-FUNC 'cauchy-w-1d))
(defun c-0-4 (x) (spline-1d x 0.4 *arr-2xN* :W-FUNC 'cauchy-w-1d))


(defun e-1-0 (x) (spline-1d x 1.0 *arr-2xN* :W-FUNC 'exp-w-1d )) 
(defun e-0-8 (x) (spline-1d x 0.8 *arr-2xN* :W-FUNC 'exp-w-1d ))
(defun e-0-6 (x) (spline-1d x 0.6 *arr-2xN* :W-FUNC 'exp-w-1d ))
(defun e-0-4 (x) (spline-1d x 0.6 *arr-2xN* :W-FUNC 'exp-w-1d ))

(defun e3-1-0 (x) (spline-1d x 1.0 *arr-2xN* :W-FUNC 'exp-3-w-1d )) 
(defun e3-0-8 (x) (spline-1d x 0.8 *arr-2xN* :W-FUNC 'exp-3-w-1d ))
(defun e3-0-6 (x) (spline-1d x 0.6 *arr-2xN* :W-FUNC 'exp-3-w-1d ))
(defun e3-0-4 (x) (spline-1d x 0.6 *arr-2xN* :W-FUNC 'exp-3-w-1d ))

(defun gnuplot-list-to-file (lst f-name)
  (with-open-file (os f-name :direction :output :if-exists :supersede )
    (mapc #'(lambda (el) (format os "~{~F ~}~%" el)) lst)))

(gnuplot-list-to-file (make-2d-list-by-func 'g-1-0 :x-from -25/10 :x-to 25/10 :step 100)  "~/gauss-1.0.data")
(gnuplot-list-to-file (make-2d-list-by-func 'g-0-8 :x-from -25/10 :x-to 25/10 :step 100)  "~/gauss-0.8.data")
(gnuplot-list-to-file (make-2d-list-by-func 'g-0-6 :x-from -25/10 :x-to 25/10 :step 100)  "~/gauss-0.6.data")
(gnuplot-list-to-file (make-2d-list-by-func 'g-0-4 :x-from -25/10 :x-to 25/10 :step 100)  "~/gauss-0.4.data")

(gnuplot-list-to-file (make-2d-list-by-func 'e-1-0 :x-from -25/10 :x-to 25/10 :step 100)  "~/exp-1.0.data")
(gnuplot-list-to-file (make-2d-list-by-func 'e-0-8 :x-from -25/10 :x-to 25/10 :step 100)  "~/exp-0.8.data")
(gnuplot-list-to-file (make-2d-list-by-func 'e-0-6 :x-from -25/10 :x-to 25/10 :step 100)  "~/exp-0.6.data")
(gnuplot-list-to-file (make-2d-list-by-func 'e-0-4 :x-from -25/10 :x-to 25/10 :step 100)  "~/exp-0.4.data")

(gnuplot-list-to-file (make-2d-list-by-func 'e3-1-0 :x-from -25/10 :x-to 25/10 :step 100)  "~/exp3-1.0.data")
(gnuplot-list-to-file (make-2d-list-by-func 'e3-0-8 :x-from -25/10 :x-to 25/10 :step 100)  "~/exp3-0.8.data")
(gnuplot-list-to-file (make-2d-list-by-func 'e3-0-6 :x-from -25/10 :x-to 25/10 :step 100)  "~/exp3-0.6.data")
(gnuplot-list-to-file (make-2d-list-by-func 'e3-0-4 :x-from -25/10 :x-to 25/10 :step 100)  "~/exp3-0.4.data")

(gnuplot-list-to-file (make-2d-list-by-func 'h-1-0 :x-from -25/10 :x-to 25/10 :step 100)  "~/hann-1.0.data")
(gnuplot-list-to-file (make-2d-list-by-func 'h-0-8 :x-from -25/10 :x-to 25/10 :step 100)  "~/hann-0.8.data")
(gnuplot-list-to-file (make-2d-list-by-func 'h-0-6 :x-from -25/10 :x-to 25/10 :step 100)  "~/hann-0.6.data")
(gnuplot-list-to-file (make-2d-list-by-func 'h-0-4 :x-from -25/10 :x-to 25/10 :step 100)  "~/hann-0.4.data")

(gnuplot-list-to-file (make-2d-list-by-func 'm-1-0 :x-from -25/10 :x-to 25/10 :step 100)  "~/mann-1.0.data")
(gnuplot-list-to-file (make-2d-list-by-func 'm-0-8 :x-from -25/10 :x-to 25/10 :step 100)  "~/mann-0.8.data")
(gnuplot-list-to-file (make-2d-list-by-func 'm-0-6 :x-from -25/10 :x-to 25/10 :step 100)  "~/mann-0.6.data")
(gnuplot-list-to-file (make-2d-list-by-func 'm-0-4 :x-from -25/10 :x-to 25/10 :step 100)  "~/mann-0.4.data")


(gnuplot-list-to-file (make-2d-list-by-func 'c-1-0 :x-from -25/10 :x-to 25/10 :step 100)  "~/cauchy-1.0.data")
(gnuplot-list-to-file (make-2d-list-by-func 'c-0-8 :x-from -25/10 :x-to 25/10 :step 100)  "~/cauchy-0.8.data")
(gnuplot-list-to-file (make-2d-list-by-func 'c-0-6 :x-from -25/10 :x-to 25/10 :step 100)  "~/cauchy-0.6.data")
(gnuplot-list-to-file (make-2d-list-by-func 'c-0-4 :x-from -25/10 :x-to 25/10 :step 100)  "~/cauchy-0.4.data")

(let ((dx 1.5))
  (gnuplot-list-to-file (loop :for x :from 0 :to 4 :by 1/50 :collect
			     (list x (spline-1d x dx *arr-2xN*)))
			"d:/PRG/msys/home/namatv/test.data")
  (gnuplot-list-to-file (loop :for x :from 0 :to 4 :by 1/50 :collect
			     (list x (spline-1d x dx *arr-2xN* :w-func #'exp-w-1d)))
			"d:/PRG/msys/home/namatv/test1.data")

  (gnuplot-list-to-file (loop :for x :from 0 :to 4 :by 1/50 :collect
			     (list x (spline-1d x dx *arr-2xN* :w-func #'cauchy-w-1d)))
			"d:/PRG/msys/home/namatv/test2.data")

  (gnuplot-list-to-file (loop :for x :from 0 :to 4 :by 1/50 :collect
			     (list x (spline-1d x dx *arr-2xN* :w-func #'hann-w-1d)))
			"d:/PRG/msys/home/namatv/test3.data")
  
  (gnuplot-list-to-file (loop :for x :from 0 :to 4 :by 1/50 :collect
			     (list x (spline-1d x dx *arr-2xN* :w-func #'mann-w-1d)))
			"d:/PRG/msys/home/namatv/test4.data")

  )


gnuplot
plot "test.data" with lines, "test1.data" with lines, "test2.data" with lines, "test3.data" with lines, "test4.data" with lines;


(/ 1 0.61805)
