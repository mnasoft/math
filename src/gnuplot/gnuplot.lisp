;;;; gnuplot.lisp

(defpackage #:math/gnuplot
  (:use #:cl #:math/core)
  (:export gnuplot-splot
	   gnuplot-plot
           table-apply
	   
           make-table
	   *palette-defined-01*

	   *pm3d-map*
           *palette-defined*
           gnuplot-data-splot
           gnuplot-data-plot
           gnuplot-data-to-file
           make-plot-data-file))

(in-package :math/gnuplot)

(defun make-2d-list-by-func (func &key (x-from 0) (x-to 1) (step 100))
  (loop :for i :from x-from :to x-to :by (/ (- x-to x-from) step)
	:collect (list (coerce i 'float) (coerce (funcall func i) 'float))))

(export 'make-table )

(defun make-table (lst-1 lst-2)
"@b(Описание:) make-table выполняет формирование списка точек, разделенного на группы.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (make-table (split-range 1.0 0.0 2) (split-range -3 0 3))
  =>  (((1.0 -3.0) (1.0 -2.0) (1.0 -1.0) (1.0 0.0))
       ((0.5 -3.0) (0.5 -2.0) (0.5 -1.0) (0.5 0.0))
       ((0.0 -3.0) (0.0 -2.0) (0.0 -1.0) (0.0 0.0)))
@end(code)
"
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

(export 'table-apply )

(defun table-apply (table func &rest second-and-others)
"@b(Описание:) функция @b(table-apply)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-table (split-range 1 4 3) (split-range 5 7 2)) 
 => 
 (((1.0 5.0) (1.0 6.0) (1.0 7.0)) 
  ((2.0 5.0) (2.0 6.0) (2.0 7.0))
  ((3.0 5.0) (3.0 6.0) (3.0 7.0)) 
  ((4.0 5.0) (4.0 6.0) (4.0 7.0)))

 (table-apply (make-table (split-range 1 4 3) (split-range 5 7 2))  #'* 10.) 
 =>
 (((1.0 5.0  50.0) (1.0 6.0  60.0) (1.0 7.0  70.0))
  ((2.0 5.0 100.0) (2.0 6.0 120.0) (2.0 7.0 140.0))
  ((3.0 5.0 150.0) (3.0 6.0 180.0) (3.0 7.0 210.0))
  ((4.0 5.0 200.0) (4.0 6.0 240.0) (4.0 7.0 280.0)))

 (table-apply (make-table (split-range 1 4 3) (split-range 5 7 2))  #'vector)
 =>
 (((1.0 5.0 #(1.0 5.0)) (1.0 6.0 #(1.0 6.0)) (1.0 7.0 #(1.0 7.0)))
  ((2.0 5.0 #(2.0 5.0)) (2.0 6.0 #(2.0 6.0)) (2.0 7.0 #(2.0 7.0)))
  ((3.0 5.0 #(3.0 5.0)) (3.0 6.0 #(3.0 6.0)) (3.0 7.0 #(3.0 7.0)))
  ((4.0 5.0 #(4.0 5.0)) (4.0 6.0 #(4.0 6.0)) (4.0 7.0 #(4.0 7.0))))
@end(code)
"
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
  ;;(table-apply-1 (make-table (split-range 1 4 3) (split-range 5 7 2))  #'*  10.) 

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

(export 'gnuplot-data-to-file )

(defgeneric gnuplot-data-to-file (f-name data)
  (:documentation "COOL"))

(defmethod gnuplot-data-to-file (f-name (data cons))
"
Тестирование:
 (gnuplot-data-to-file \"~/data.data\" '((0 0)(1 1)(2 4)(3 9)(4 16)))
"
  (with-open-file (os f-name :direction :output :if-exists :supersede :external-format :utf8)
    (mapc #'(lambda (el) (format os "~{~F ~}~%" el)) data)
    (format t "gnuplot -e \"plot '~A'\"  " f-name)))

(defmethod gnuplot-data-to-file (f-name (data array))
"Тестирование:
 (gnuplot-data-to-file \"~/data.data\" (make-array '(5 2) :initial-contents '((0 0)(1 1)(2 4)(3 9)(4 16))))
"
  (assert (= (array-rank data) 2))
  (with-open-file (os f-name :direction :output :if-exists :supersede :external-format :utf8)
    (loop :for i :from 0 :below (array-dimension data 0) :do
	 (loop :for j :from 0 :below (array-dimension data 1) :do
	      (format os "~F " (aref data i j)))
	 (format os "~%" ))
    (format t "gnuplot -e \"plot '~A'\"  " f-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '*palette-defined*)

(defparameter *palette-defined* "set palette defined (0.05 'blue', 0.2 'cyan', 0.4 'green', 0.6 'yellow', 0.8 'orange', 1.0 'red')"
  "STUB")

(export '*palette-defined-01*)

(defparameter *palette-defined-01*
  "set palette defined (0 'blue', 0.1 'white', 0.2 'cyan', 0.3 'white', 0.4 'green', 0.5 'white', 0.6 'yellow', 0.7 'white', 0.8 'orange', 0.9 'white', 1 'red')"
  "STUB")

(export '*pm3d-map*)

(defparameter *pm3d-map* "set pm3d map"
  "STUB")

(export 'gnuplot-splot )

(defun gnuplot-splot (f-name
		      &key
			(terminal "set terminal pdfcairo enhanced font 'Arial,14' size 13.5 cm, 5.0 cm ")
			(output   (concatenate 'string "set output '" f-name ".pdf'"))
			(preamble  nil)
			(palette  *palette-defined*) 
			(pm3d     *pm3d-map*)
			(splot    (concatenate 'string "splot '" f-name ".data' u 2:1:3")))
  "Осуществляет подготовку данных, содержащихся в файле f-name с расширением data.
Данные в файле должны иметь формат gp-list
"
  (assert (probe-file (concatenate 'string f-name ".data")))
  (with-open-file (gp (concatenate 'string f-name "." "gp") :direction :output :if-exists :supersede :external-format :utf8)
    (when terminal (format gp "~A~%" terminal))
    (when preamble (format gp "~A~%" preamble))
    (when output   (format gp "~A~%" output)) 
    (when palette  (format gp "~A~%" palette))
    (when pm3d     (format gp "~A~%" pm3d))
    (when splot    (format gp "~A~%" splot)))
  (with-open-file (sh (concatenate 'string f-name "." "sh") :direction :output :if-exists :supersede :external-format :utf8)
    (format sh "#!/bin/bash~%" )
    (format sh "gnuplot ~A.gp~%" f-name))
  (uiop:run-program (concatenate 'string "sh" " " f-name "." "sh") :ignore-error-status t))

(export 'gnuplot-data-splot )

(defun gnuplot-data-splot (
			   f-name data &key
			   (terminal "set terminal pdfcairo enhanced font 'Arial,14' size 13.5 cm, 5.0 cm ")
			   (output   (concatenate 'string "set output '" f-name ".pdf'"))
			   (preamble  nil)
			   (palette  *palette-defined*) 
			   (pm3d     *pm3d-map*)
					 (splot    (concatenate 'string "splot '" f-name ".data' u 2:1:3")))
  "STUB"
  (assert (consp data))
  (assert (consp (first data)))
  (assert (consp (first (first data))))
  (with-open-file (os (concatenate 'string f-name "." "data") :direction :output :if-exists :supersede :external-format :utf8)
    (format os "#   ~8A ~8A ~8A~%" "X" "Y" "Z" )
    (format os "~{~{~{~8F ~}~%~}~%~}"     data ))
  (with-open-file (gp (concatenate 'string f-name "." "gp") :direction :output :if-exists :supersede :external-format :utf8)
    (when terminal (format gp "~A~%" terminal))
    (when preamble (format gp "~A~%" preamble))
    (when output   (format gp "~A~%" output)) 
    (when palette  (format gp "~A~%" palette))
    (when pm3d     (format gp "~A~%" pm3d))
    (when splot    (format gp "~A~%" splot)))
  (with-open-file (sh (concatenate 'string f-name "." "sh") :direction :output :if-exists :supersede :external-format :utf8)
    (format sh "#!/bin/bash~%" )
    (format sh "gnuplot ~A.gp~%" f-name))
  (uiop:run-program (concatenate 'string "sh" " " f-name "." "sh") :ignore-error-status t))

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


(export 'gnuplot-data-plot )

(defun gnuplot-data-plot (f-name data &key
			  (terminal "set terminal pngcairo size 1400,500 enhanced font 'Verdana,10'")
			  (preamble "set xrange [0:2.5]")
			  (output   (concatenate 'string "set output '" f-name ".png'"))
			  (plot    (concatenate 'string "plot '" f-name ".data' u 2:1")))
"@b(Описание:) функция @b(gnuplot-data-plot)

 @b(Пример использования:)
@begin[lang=lisp](code)
;;;; Пример 1
 (math:gnuplot-data-plot
 \"plot2\"
 (mapcar #'(lambda (x) (list x (sin x)(sqrt x)) )
	 (math:split-range 0.0 10 1000) )
 :plot \"plot 'plot2.data' u 1:2 with lines lt 1, 'plot2.data' u 1:3 with lines lt 2 \")
@end(code)
"
;;;; :plot "plot 'plot2.data' u 1:2 with lines lt 1, 'plot2.data' u 1:3 with lines lt 2 ") 
  (assert (consp data))
  (assert (consp (first data)))
  (with-open-file (os (concatenate 'string f-name "." "data") :direction :output :if-exists :supersede :external-format :utf8)
    (format os "#   ~8A ~8A~%" "X" "Y" )
    (format os "~{~{~8F ~}~%~}"     data ))
  (with-open-file (gp (concatenate 'string f-name "." "gp") :direction :output :if-exists :supersede :external-format :utf8)
    (when terminal (format gp "~A~%" terminal))
    (when preamble (format gp "~A~%" preamble)) 
    (when output   (format gp "~A~%" output)) 
    (when plot    (format gp "~A~%" plot)))
  (with-open-file (sh (concatenate 'string f-name "." "sh") :direction :output :if-exists :supersede :external-format :utf8)
    (format sh "#!/bin/bash~%" )
    (format sh "gnuplot ~A.gp~%" f-name))
  (uiop:run-program (concatenate 'string "sh" " " f-name "." "sh") :ignore-error-status t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'gnuplot-plot )

(defun gnuplot-plot (f-name &key
		     (terminal "set terminal enhanced font 'Arial,14' pdfcairo size 42 cm, 29.7 cm")
		     (preamble "set xrange [0:2.5]")
		     (output   (concatenate 'string "set output '" f-name ".pdf'"))
			      (plot    (concatenate 'string "plot '" f-name ".data' u 2:1")))
"STUB"
  (with-open-file (gp (concatenate 'string f-name "." "gp") :direction :output :if-exists :supersede :external-format :utf8)
    (when terminal (format gp "~A~%" terminal))
    (when preamble (format gp "~A~%" preamble)) 
    (when output   (format gp "~A~%" output)) 
    (when plot    (format gp "~A~%" plot)))
  (with-open-file (sh (concatenate 'string f-name "." "sh") :direction :output :if-exists :supersede :external-format :utf8)
    (format sh "#!/bin/bash~%" )
    (format sh "gnuplot ~A.gp~%" f-name))
  (uiop:run-program (concatenate 'string "sh" " " f-name "." "sh") :ignore-error-status t))

(export 'make-plot-data-file )

(defun make-plot-data-file (f-name data)
"@b(Описание:) функция @b(make-plot-data-file)

 @b(Пример использования:)
@begin[lang=lisp](code)
;;;; Пример 1
 (make-plot-data-file
  \"plot2\"
  (mapcar
   #'(lambda (x)
       (list x (sin x) (sqrt x)))
   (math:split-range 0.0 10 1000)))
@end(code)
"
  (assert (consp data))
  (assert (consp (first data)))
  (with-open-file (os (concatenate 'string f-name "." "data") :direction :output :if-exists :supersede :external-format :utf8)
    (format os "~{~{~8F ~}~%~}" data )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass gnuplot-vector ()
  ((orign     :accessor gnuplot-vector-origin    :initform (vector 0.0 0.0 0.0) :initarg :orign)
   (direction :accessor gnuplot-vector-direction :initform (vector 1.0 0.0 0.0) :initarg :direction)))

(defmethod print-object ((v gnuplot-vector) s)
  (format s "~{~A ~}" (coerce (gnuplot-vector-origin    v) 'list ))
  (format s "~{~A ~}" (coerce (gnuplot-vector-direction v) 'list )))

(defgeneric move (obj displacement))

(defgeneric move (obj displacement))

(defmethod move ((obj gnuplot-vector) (displacement vector))
  (with-slots (orign) obj
    (assert (= (length displacement) (length orign )))
    (loop :for i :from 0 :below (length (gnuplot-vector-origin obj)) :do
	 (setf (svref orign i)  (+ (svref orign i)  (svref displacement i))))
    obj))

(defmethod rotate ((obj gnuplot-vector) (angle number) (o vector))
  (with-slots (orign) obj
    (assert (= (length displacement) (length orign )))
    (loop :for i :from 0 :below (length (gnuplot-vector-origin obj)) :do
	 (setf (svref orign i)  (+ (svref orign i)  (svref displacement i))))
    obj))

;(make-instance 'matrix :dimensions '(3 3))

;(make-instance 'vector 

;(defparameter *gp-v* (make-instance 'gnuplot-vector ))

;(move  *gp-v* #(-10. -10. -10.))
