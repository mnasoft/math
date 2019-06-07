;;;; gnuplot.lisp

(in-package #:math)

(defun make-2d-list-by-func (func &key (x-from 0) (x-to 1) (step 100))
    (loop :for i :from x-from :to x-to :by (/ (- x-to x-from) step) :collect
	 (list (coerce i 'float) (coerce (funcall func i) 'float))))

(export 'split-range)
(defun split-range (from to steps)
  "Пример использования:
 (split-range 10 20 5) =>(10.0 12.0 14.0 16.0 18.0 20.0) "
      (loop :for i :from 0 :to steps :collect
	   (coerce (+ from (* (/ i steps ) (- to from))) 'float)))


(export 'split-range-by-func)
(defun split-range-by-func (from to steps &key
					    (func #'(lambda (x) (log x 10)))
					    (anti-func #'(lambda (x) (expt 10 x))))
  "Пример использования:
 (split-range-by-func 1 10 5) => (1.0 1.5848932 2.5118864 3.981072 6.3095737 10.0) "
  (mapcar
   #'(lambda (el)(funcall anti-func el))
   (split-range (funcall func from) (funcall func to) steps)))

(export 'make-table)
(defun make-table (lst-1 lst-2)
  "Выполняет формирование списка точек, разделенного на группы.
Пример использования:
 (make-table (split-range 1.0 0.0 2) (split-range -3 0 3)) =>
 (((1.0 -3.0) (1.0 -2.0) (1.0 -1.0) (1.0 0.0))
  ((0.5 -3.0) (0.5 -2.0) (0.5 -1.0) (0.5 0.0))
  ((0.0 -3.0) (0.0 -2.0) (0.0 -1.0) (0.0 0.0)))"
  (assert (consp lst-1)) 
  (assert (consp lst-2))
  (assert (not (find-if-not #'numberp lst-1)))
  (assert (not (find-if-not #'numberp lst-2)))
  (labels ((v-lst (val lst)
	     (mapcar
	      #'(lambda (el) (list val el))
	      lst)))
    (mapcar
     #'(lambda (el-1) (v-lst el-1 lst-2))
     lst-1)))

(export 'table-apply)
(defun table-apply (table func &rest second-and-others &key )
  "Пример использования:
  (table-apply (make-table (split-range 1 4 3) (split-range 5 7 2))  #'* 10.) =>
  (table-apply (make-table (split-range 1 4 3) (split-range 5 7 2))  #'vector)

 (((1.0 5.0 #(1.0 5.0)) (1.0 6.0 #(1.0 6.0)) (1.0 7.0 #(1.0 7.0)))
 ((2.0 5.0 #(2.0 5.0)) (2.0 6.0 #(2.0 6.0)) (2.0 7.0 #(2.0 7.0)))
 ((3.0 5.0 #(3.0 5.0)) (3.0 6.0 #(3.0 6.0)) (3.0 7.0 #(3.0 7.0)))
 ((4.0 5.0 #(4.0 5.0)) (4.0 6.0 #(4.0 6.0)) (4.0 7.0 #(4.0 7.0))))

 (((1.0 5.0 50.0) (1.0 6.0 60.0) (1.0 7.0 70.0))
  ((2.0 5.0 100.0) (2.0 6.0 120.0) (2.0 7.0 140.0))
  ((3.0 5.0 150.0) (3.0 6.0 180.0) (3.0 7.0 210.0))
  ((4.0 5.0 200.0) (4.0 6.0 240.0) (4.0 7.0 280.0)))"
  (assert (consp table))
  (mapcar
   #'(lambda (el)
       (mapcar
	#'(lambda (el-1)
	    (append el-1 (list (apply func (append el-1 second-and-others)))))
	el))
   table))

(defun table-apply-0 (table func &rest second-and-others &key )
  "Пример использования:
   (table-apply-0 (make-table (split-range 1 4 3) (split-range 5 7 2))  #'vector) =>
   ((#(1.0 5.0) #(1.0 6.0) #(1.0 7.0))
    (#(2.0 5.0) #(2.0 6.0) #(2.0 7.0))
    (#(3.0 5.0) #(3.0 6.0) #(3.0 7.0))
    (#(4.0 5.0) #(4.0 6.0) #(4.0 7.0))) "
  (assert (consp table))
  (mapcar
   #'(lambda (el)
       (mapcar
	#'(lambda (el-1)
	    (apply func (append el-1 second-and-others)))
	el))
   table))

(defun table-apply-1 (table func &rest second-and-others)
  "Пример использования:
  ;;(table-apply (make-table (split-range 1 4 3) (split-range 5 7 2))  #'*  10.) 

=>
 (((1.0 5.0 50.0) (1.0 6.0 60.0) (1.0 7.0 70.0))
  ((2.0 5.0 100.0) (2.0 6.0 120.0) (2.0 7.0 140.0))
  ((3.0 5.0 150.0) (3.0 6.0 180.0) (3.0 7.0 210.0))
  ((4.0 5.0 200.0) (4.0 6.0 240.0) (4.0 7.0 280.0)))"
  (assert (consp table))
  (mapcar
   #'(lambda (el)
       (mapcar
	#'(lambda (el-1)
	    (append el-1 (list (apply func (cons (apply #' vector el-1) second-and-others)))))
	el))
   table))

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

(export 'make-t-fild-data)
(defun make-t-fild-data (a-temperatures r-hights r-ocr 
			 &key (d-pts (vector 0.2 0.5)) (delta 0.1) (hights '(1 0 100)) (ocr '(-140/100 140/100 100)))
;;;; *default-pathname-defaults*
  (let* ((pts-vals (multiple-value-list (make-points-values a-temperatures r-hights r-ocr)))
	 (pts  (first pts-vals))
	 (vals (second  pts-vals))
	 (r-vals (refine-approximation-values pts vals d-pts :delta delta)))
    (table-apply-1
     (make-table (apply #'split-range hights) (apply #'split-range ocr))
     #'approx-by-points d-pts pts r-vals)))

(export 'gnuplot-data-splot)
(defun gnuplot-data-splot (
			   f-name data &key
			   (terminal "set terminal pngcairo size 1400,500 enhanced font 'Verdana,10'")
			   (output   (concatenate 'string "set output '" f-name ".png'"))
			   (palette  "set palette defined (0 'blue', 0.1 'white', 0.2 'cyan', 0.3 'white', 0.4 'green', 0.5 'white', 0.6 'yellow', 0.7 'white', 0.8 'orange', 0.9 'white', 1 'red')")
			   (pm3d     "set pm3d map")
			   (splot    (concatenate 'string "splot '" f-name ".data' u 2:1:3")))
  (assert (consp data))
  (assert (consp (first data)))
  (assert (consp (first (first data))))
  (with-open-file (os (concatenate 'string f-name "." "data") :direction :output :if-exists :supersede)
    (format os "#   ~8A ~8A ~8A~%" "X" "Y" "Z" )
    (format os "~{~{~{~8F ~}~%~}~%~}"     data ))
  (with-open-file (gp (concatenate 'string f-name "." "gp") :direction :output :if-exists :supersede)
    (when terminal (format gp "~A~%" terminal)) 
    (when output   (format gp "~A~%" output)) 
    (when palette  (format gp "~A~%" palette))
    (when pm3d     (format gp "~A~%" pm3d))
    (when splot    (format gp "~A~%" splot)))
  (with-open-file (sh (concatenate 'string f-name "." "sh") :direction :output :if-exists :supersede)
    (format sh "#!/bin/bash~%" )
    (format sh "gnuplot ~A.gp~%" f-name))
  (uiop:run-program (concatenate 'string "sh" " " f-name "." "sh") :ignore-error-status t))




;;;;
;;;; set terminal pngcairo size 1000,600 enhanced font 'Verdana,10'
;;;; set output 'introduction.png'
;;;; set palette defined (0 'blue', 0.1 'white', 0.2 'cyan', 0.3 'white', 0.4 'green', 0.5 'white', 0.6 'yellow', 0.7 'white', 0.8 'orange', 0.9 'white', 1 'red')
;;;; set pm3d map
;;;; splot '~/splot.data' u 2:1:3
;;;; exit

;;;; gnuplot splot 'splot.data' u 1:2:3 w l
;;;; splot "grid" u 1:2:3 w l
;;;; splot "matrix" nonuniform matrix u 1:2:3 w l
;;;; pos( k, origin, spacing ) = origin + k*spacing
;;;; splot "packedmatrix" matrix u (pos($1,0,1)):(pos($2,-1,1)):3 w l
