;;;; gnuplot.lisp

(in-package #:math)

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
