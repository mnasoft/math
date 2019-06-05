;;;; gnuplot.lisp

(in-package #:math)

(defun make-2d-list-by-func (func &key (x-from 0) (x-to 1) (step 100))
    (loop :for i :from x-from :to x-to :by (/ (- x-to x-from) step) :collect
	 (list (coerce i 'float) (coerce (funcall func i) 'float))))

(defun split-range (from to steps)
  "Пример использования:
 (split-range 10 20 5) =>(10.0 12.0 14.0 16.0 18.0 20.0) "
      (loop :for i :from 0 :to steps :collect
	   (coerce (+ from (* (/ i steps ) (- to from))) 'float)))



(defun split-range-by-func (from to steps &key
					    (func #'(lambda (x) (log x 10)))
					    (anti-func #'(lambda (x) (expt 10 x))))
  "Пример использования:
 (split-range-by-func 1 10 5) => (1.0 1.5848932 2.5118864 3.981072 6.3095737 10.0) "
  (mapcar
   #'(lambda (el)(funcall anti-func el))
   (split-range (funcall func from) (funcall func to) steps)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'gnuplot-data-to-file)
(defgeneric gnuplot-data-to-file (f-name data)
  (:documentation "COOL"))

(defmethod gnuplot-data-to-file (f-name (data cons))
  "
Тестирование:
 (gnuplot-data-to-file \"~/data.data\" '((0 0)(1 1)(2 4)(3 9)(4 16)))
"
  (with-open-file (os f-name :direction :output :if-exists :supersede )
    (mapc #'(lambda (el) (format os "~{~F ~}~%" el)) data)
    (format t "gnuplot -e \"plot '~A'\"  " f-name)))

(defmethod gnuplot-data-to-file (f-name (data array))
  "
Тестирование:
 (gnuplot-data-to-file \"~/data.data\" (make-array '(5 2) :initial-contents '((0 0)(1 1)(2 4)(3 9)(4 16))))
"
  (assert (= (array-rank data) 2))
  (with-open-file (os f-name :direction :output :if-exists :supersede )
    (loop :for i :from 0 :below (array-dimension data 0) :do
	 (loop :for j :from 0 :below (array-dimension data 1) :do
	      (format os "~F " (aref data i j)))
	 (format os "~%" ))
    (format t "gnuplot -e \"plot '~A'\"  " f-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'gnuplot-data-splot)
(defun gnuplot-data-splot (f-name data)
  (assert (consp data))
  (assert (consp (first data)))
  (assert (consp (first (first data))))
  (with-open-file (os f-name :direction :output :if-exists :supersede)
    (format os "#   ~8A ~8A ~8A~%" "X" "Y" "Z" )
    (format os "~{~{~{~8F ~}~%~}~%~}"     data )
    (format t "set palette defined (0 'blue', 0.1 'white', 0.2 'cyan', 0.3 'white', 0.4 'green', 0.5 'white', 0.6 'yellow', 0.7 'white', 0.8 'orange', 0.9 'white', 1 'red')~%")
    (format t "set pm3d map~%")
    (format t "splot '~A' u 1:2:3~%" f-name)))
