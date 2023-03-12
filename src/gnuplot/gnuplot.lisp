;;;; ./src/gnuplot/package.lisp

(defpackage :math/gnuplot
  (:use #:cl #:math/core)
  (:export gnuplot-splot
	   gnuplot-plot
           table-apply
	   
           make-table
           *default-gnuplot-direcroty*
	   *palette-defined-01*

	   *pm3d-map*
           *palette-defined*
           gnuplot-data-splot
           gnuplot-data-plot
           gnuplot-data-to-file
           make-plot-data-file
           rgb
           *term-pngcairo*
           *term-pdfcairo*

           ))

(in-package :math/gnuplot)

(defparameter *default-gnuplot-direcroty*
  (ensure-directories-exist #P"~/gnuplot/")
    "Каталог для вывода по умолчанию.")

;;;; (directory-namestring *default-gnuplot-direcroty*)

(defun file-name (f-name &optional (f-ext ""))
    "Определяет имя файла в каталоге поумолчанию."
  (assert (stringp f-name))
  (assert (stringp f-ext))
  (if (string= "" f-ext)
      (concatenate 'string (directory-namestring *default-gnuplot-direcroty*)
                   f-name f-ext)
      (concatenate 'string (directory-namestring *default-gnuplot-direcroty*)
                   f-name "." f-ext)))

(defun find-font-family (&key (family "Times New Roman"))
    "@b(Описание:) функция @b(find-font-family) возвращает имя шрифта,
установленного в систему похожего на тот, что указан.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (find-font-family :family \"Arial\")
 (find-font-family :family \"Courier New\")
 (find-font-family :family \"Times New Roman\")
@end(code)
"
 (org.shirakumo.font-discovery:family
  (org.shirakumo.font-discovery:find-font :family family)))

(defun rgb (aa rr gg bb)
    "@b(Описание:) функция @b(rgb) возвращает строковое представление цвета.

 @b(Переменые:)
@begin(list)
@iterm(aa = 0..255 яркость;) 
@iterm(rr = 0..255 насыщенность красного;) 
@iterm(gg = 0..255 насыщенность зеленого;) 
@iterm(bb = 0..255 насыщенность синего.) 
@end(list)
"
  (assert (and (<= 0 aa 255) (<= 0 rr 255) (<= 0 gg 255) (<= 0 bb 255)))
  (format nil "#~X~X~X~X" aa rr gg bb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <term> ()
  ((no-enhanced          :initarg no-enhanced          :initform :enhanced)
   (mono/color           :initarg mono/color           :initform :mono)
   (font                 :initarg font                 :initform (find-font-family :family "Times New Roman"))
   (fontscale            :initarg fontscale            :initform 12)
   (linewidth            :initarg linewidth            :initform 0.5)
   (rounded/butt/square  :initarg rounded/butt/square  :initform :rounded)
   (dashlength           :initarg dashlength           :initform 1.0)
   (background           :initarg background           :initform (rgb #xFF #xFF #xFF #xFF))
   (size                 :initarg size                 :initform '(15 10))
   (size-units
    :initarg size-units
    :initform :cm
    :documentation
    "По умолчанию - пиксели|пиксели=72*in|пиксели=72/25.4*cm")
   ))

(defclass term-pdfcairo (<term>) ())

(defclass term-pngcairo (<term>)
  ((no-transparent :initarg no-transparent :initform :no-transparent)
   (no-crop        :initarg no-crop        :initform :no-crop)
   (pointscale     :initarg pointscale     :initform 1.0))
  (:documentation
   " @b(Пример использования:)
@begin[lang=gnuplot](code)
 set term pngcairo
             {{no}enhanced} {mono|color}
             {{no}transparent} {{no}crop} {background <rgbcolor>}
             {font <font>} {fontscale <scale>}
             {linewidth <lw>} {rounded|butt|square} {dashlength <dl>}
             {pointscale <ps>}
             {size <XX>{unit},<YY>{unit}}
@end(code)
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *term-pdfcairo* (make-instance 'term-pdfcairo))

(defparameter *term-pngcairo* (make-instance 'term-pngcairo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((term term-pdfcairo) os)
  (format os "~a"
          (with-output-to-string (stream)
            (format stream "set term pdfcairo")
            (block no-enhanced
              (when (eq :enhanced (slot-value term 'no-enhanced))
                (format stream " enhanced"))
              (when (eq :no-enhanced (slot-value term 'no-enhanced))
                (format stream " noenhanced")))
            (block mono/color
              (when (eq :mono (slot-value term 'mono/color))
                (format stream " mono"))
              (when (eq :color (slot-value term 'mono/color))
                (format stream " color")))
            (block font-fontscale
              (when (and (slot-value term 'font) (slot-value term 'fontscale))
                (format stream " font \"~a, ~d\"" (slot-value term 'font) (slot-value term 'fontscale))))
            (when (slot-value term 'linewidth)
              (format stream " linewidth ~a" (slot-value term 'linewidth)))
            (block rounded/butt/square
              (when (eq :rounded (slot-value term 'rounded/butt/square))
                (format stream " rounded"))
              (when (eq :butt (slot-value term 'rounded/butt/square))
                (format stream " butt"))
              (when (eq :square (slot-value term 'rounded/butt/square))
                (format stream " square")))
            (when (slot-value term 'dashlength)
              (format stream " dashlength ~a" (slot-value term 'dashlength)))
            (when (slot-value term 'background)
              (format stream " background ~s" (slot-value term 'background)))
            (block size/size-units
              (when (and (slot-value term 'size)
                         (slot-value term 'size-units)
                         (eq :in (slot-value term 'size-units)))
                (format stream " size ~ain,~ain"
                        (first (slot-value term 'size))
                        (second (slot-value term 'size))))
              (when (and (slot-value term 'size)
                         (slot-value term 'size-units)
                         (eq :cm (slot-value term 'size-units)))
                (format stream " size ~acm,~acm"
                        (first (slot-value term 'size))
                        (second (slot-value term 'size)))))
            (format stream ";~%"))))

(defmethod print-object ((term term-pngcairo) os)
  (format os "~a"
          (with-output-to-string (stream)
            (format stream "set term pngcairo")
            (block no-enhanced
              (when (eq :enhanced (slot-value term 'no-enhanced))
                (format stream " enhanced"))
              (when (eq :no-enhanced (slot-value term 'no-enhanced))
                (format stream " noenhanced")))
            (block mono/color
              (when (eq :mono (slot-value term 'mono/color))
                (format stream " mono"))
              (when (eq :color (slot-value term 'mono/color))
                (format stream " color")))
            (block font-fontscale
              (when (and (slot-value term 'font) (slot-value term 'fontscale))
                (format stream " font \"~a, ~d\"" (slot-value term 'font) (slot-value term 'fontscale))))
            (when (slot-value term 'linewidth)
              (format stream " linewidth ~a" (slot-value term 'linewidth)))
            (block rounded/butt/square
              (when (eq :rounded (slot-value term 'rounded/butt/square))
                (format stream " rounded"))
              (when (eq :butt (slot-value term 'rounded/butt/square))
                (format stream " butt"))
              (when (eq :square (slot-value term 'rounded/butt/square))
                (format stream " square")))
            (when (slot-value term 'dashlength)
              (format stream " dashlength ~a" (slot-value term 'dashlength)))
            (when (slot-value term 'background)
              (format stream " background ~s" (slot-value term 'background)))
            (block no-transparent
              (when (eq :no-transparent (slot-value term 'no-transparent))
                (format stream " notransparent"))
              (when (eq :transparent (slot-value term 'no-transparent))
                (format stream " transparent")))
            (block no-crop
              (when (eq :no-crop (slot-value term 'no-crop))
                (format stream " nocrop"))
              (when (eq :crop (slot-value term 'no-crop))
                (format stream " crop")))
            (block pointscale
              (when (slot-value term 'pointscale)
                (format stream " pointscale ~a" (slot-value term 'pointscale))))
            (block size/size-units
              (when (and (slot-value term 'size)
                         (null (slot-value term 'size-units)))
                (format stream " size ~a,~a"
                        (first (slot-value term 'size))
                        (second (slot-value term 'size))))
              (when (and (slot-value term 'size)
                         (slot-value term 'size-units)
                         (eq :in (slot-value term 'size-units)))
                (format stream " size ~ain,~ain"
                        (first (slot-value term 'size))
                        (second (slot-value term 'size))))
              (when (and (slot-value term 'size)
                         (slot-value term 'size-units)
                         (eq :cm (slot-value term 'size-units)))
                (format stream " size ~acm,~acm"
                        (first (slot-value term 'size))
                        (second (slot-value term 'size)))))
            (format stream ";~%")
            )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output ((term term-pdfcairo) (f-name string) os)
  (format os "~a" 
          (with-output-to-string (stream)
            (format stream "set output '~a'" (file-name f-name "pdf")))))

;;;; (get-output-stream-string stream) (format os "~a" )

(defmethod output ((term term-pngcairo) (f-name string) os)
  (format os "~a" 
          (with-output-to-string (stream)
            (format stream "set output '~a'" (file-name f-name "png")))))

;;;; (get-output-stream-string stream) (format os "~a" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format nil "~a" *term-pngcairo*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; ./src/gnuplot/gnuplot.lisp

(defun make-2d-list-by-func (func &key (x-from 0) (x-to 1) (steps 100))
  (mapcar #'(lambda (el) (list el (funcall func el)))
          (split-range x-from x-to steps)))

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

  "@b(Описание:) обобщенная функция @b(gnuplot-data-to-file) выводит данные
@b(data) в файл с именем @b(f-name), расположенный в каталоге поумолчанию
(см. переменную *default-gnuplot-direcroty*)."(defgeneric gnuplot-data-to-file (f-name data))

(defmethod gnuplot-data-to-file (f-name (data cons))
    "@b(Описание:) метод @b(gnuplot-data-to-file) выводит данные
@b(data) в файл с именем @b(f-name), расположенный в каталоге поумолчанию
(см. переменную *default-gnuplot-direcroty*). 

 Данные должны быть представлены 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gnuplot-data-to-file \"data\" 
  (loop :for i :from 0 :to 4 :by 1/10 :collect (list i (* i i))))
@end(code)
"
  (with-open-file (os (file-name f-name "data") :direction :output :if-exists :supersede :external-format :utf8)
    (mapc #'(lambda (el) (format os "~{~F ~}~%" el)) data)
    (format t "'~A'" (file-name f-name "data"))))

;;;; (gnuplot-data-to-file "data-2d-list" (loop :for i :from 0 :to 4 :by 1/10 :collect (list i (* i i))))

(defmethod gnuplot-data-to-file (f-name (data array))
    "@b(Описание:) метод @b(gnuplot-data-to-file) выводит данные
@b(data) в файл с именем @b(f-name), расположенный в каталоге поумолчанию
(см. переменную *default-gnuplot-direcroty*). 

 Данные должны быть представлены 2d-array.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gnuplot-data-to-file \"data\" 
  (loop :for i :from 0 :to 4 :by 1/10 :collect (list i (* i i))))
 (gnuplot-data-to-file \"data\" (make-array '(5 2) :initial-contents '((0 0)(1 1)(2 4)(3 9)(4 16))))
@end(code)
"
  (assert (= (array-rank data) 2))
  (with-open-file (os (file-name f-name "data") :direction :output :if-exists :supersede :external-format :utf8)
    (loop :for i :from 0 :below (array-dimension data 0) :do
      (loop :for j :from 0 :below (array-dimension data 1) :do
	(format os "~F " (aref data i j)))
      (format os "~%" ))
    (format t "'~A'" (file-name f-name "data"))))

;;;; (gnuplot-data-to-file "data-array" (make-array '(5 2) :initial-contents '((0 0)(1 1)(2 4)(3 9)(4 16))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *palette-defined*
  "set palette defined (0.05 'blue', 0.2 'cyan', 0.4 'green', 0.6 'yellow', 0.8 'orange', 1.0 'red')"
  "STUB")

(defparameter *palette-defined-01*
  "set palette defined (0 'blue', 0.1 'white', 0.2 'cyan', 0.3 'white', 0.4 'green', 0.5 'white', 0.6 'yellow', 0.7 'white', 0.8 'orange', 0.9 'white', 1 'red')"
  "STUB")

(defparameter *pm3d-map* "set pm3d map"
  "STUB")

(defun gnuplot-splot (f-name
		      &key
			(terminal "set terminal pdfcairo enhanced font 'Arial,14' size 13.5 cm, 5.0 cm ")
			(output   (concatenate 'string "set output '" f-name ".pdf'"))
			(preamble  nil)
			(palette  *palette-defined*) 
			(pm3d     *pm3d-map*)
			(splot    (concatenate 'string "splot '" (file-name f-name "data") "' u 2:1:3")))
  "Осуществляет подготовку данных, содержащихся в файле f-name с расширением data.
Данные в файле должны иметь формат gp-list."
  (assert (probe-file (file-name f-name "data")))
  (with-open-file (gp (file-name f-name "gp") :direction :output :if-exists :supersede :external-format :utf8)
    (when terminal (format gp "~A~%" terminal))
    (when preamble (format gp "~A~%" preamble))
    (when output   (format gp "~A~%" output)) 
    (when palette  (format gp "~A~%" palette))
    (when pm3d     (format gp "~A~%" pm3d))
    (when splot    (format gp "~A~%" splot)))
  (with-open-file (sh (file-name f-name "sh") :direction :output :if-exists :supersede :external-format :utf8)
    (format sh "#!/bin/bash~%" )
    (format sh "gnuplot ~A.gp~%" f-name))
  (uiop:run-program (concatenate 'string "sh" " " f-name "." "sh") :ignore-error-status t))

(defun gnuplot-data-splot (
			   f-name data &key
			   (terminal "set terminal pdfcairo enhanced font 'Arial,14' size 13.5 cm, 5.0 cm ")
			   (output   (concatenate 'string "set output '" (file-name f-name "pdf'")))
			   (preamble  nil)
			   (palette  *palette-defined*) 
			   (pm3d     *pm3d-map*)
                                         (splot    (concatenate 'string "splot '" (file-name f-name "data")"' u 2:1:3")))
    "STUB"
  (assert (consp data))
  (assert (consp (first data)))
  (assert (consp (first (first data))))
  (with-open-file (os (file-name f-name "data") :direction :output :if-exists :supersede :external-format :utf8)
    (format os "#   ~8A ~8A ~8A~%" "X" "Y" "Z" )
    (format os "~{~{~{~8F ~}~%~}~%~}"     data ))
  (with-open-file (gp (file-name  f-name "gp") :direction :output :if-exists :supersede :external-format :utf8)
    (when terminal (format gp "~A~%" terminal))
    (when preamble (format gp "~A~%" preamble))
    (when output   (format gp "~A~%" output)) 
    (when palette  (format gp "~A~%" palette))
    (when pm3d     (format gp "~A~%" pm3d))
    (when splot    (format gp "~A~%" splot)))
  (with-open-file (sh (file-name f-name "sh") :direction :output :if-exists :supersede :external-format :utf8)
    (format sh "#!/bin/bash~%" )
    (format sh "gnuplot ~A~%" (file-name f-name "gp")))
  (uiop:run-program (concatenate 'string "sh" " " (file-name f-name "sh")) :ignore-error-status t))

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

(defun gnuplot-data-plot (f-name data
                          &key
			    (terminal (format nil "~a" *term-pdfcairo*))
                            (output   (concatenate 'string "set output '" (file-name f-name "pdf") "'"))
			    (preamble "set xrange [0:4]")
			    (plot    (concatenate 'string "plot '" (file-name f-name "data")"' u 1:2 with lines")))
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
  (assert (consp data))
  (assert (consp (first data)))
  (with-open-file (os (file-name f-name "data") :direction :output :if-exists :supersede :external-format :utf8)
    (format os "#   ~8A ~8A~%" "X" "Y" )
    (format os "~{~{~8F ~}~%~}"     data ))
  (with-open-file (gp (file-name f-name "gp") :direction :output :if-exists :supersede :external-format :utf8)
    (when terminal (format gp "~A~%" terminal))
    (when preamble (format gp "~A~%" preamble)) 
    (when output   (format gp "~A~%" output)) 
    (when plot     (format gp "~A~%" plot)))
  (with-open-file (sh (file-name  f-name "sh") :direction :output :if-exists :supersede :external-format :utf8)
    (format sh "#!/bin/bash~%" )
    (format sh "gnuplot ~A~%" (file-name  f-name "gp")))
  (uiop:run-program (concatenate 'string "sh" " " (file-name f-name "sh")) :ignore-error-status t))

;;;; (gnuplot-data-plot "123" (make-2d-list-by-func #'(lambda (el) (* el el)) :x-from 0 :x-to 4 :steps 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gnuplot-plot (f-name &key
		              (terminal (format nil "~a" *term-pdfcairo*))
		              (preamble "set xrange [0:2.5]")
		              (output   (concatenate 'string "set output '" (file-name f-name ".pdf") "'"))
			      (plot    (concatenate 'string "plot '" (file-name f-name ".data") "' u 2:1")))
  "STUB"
  (with-open-file (gp (file-name f-name ".gp")  :direction :output :if-exists :supersede :external-format :utf8)
    (when terminal (format gp "~A~%" terminal))
    (when preamble (format gp "~A~%" preamble)) 
    (when output   (format gp "~A~%" output)) 
    (when plot    (format gp "~A~%" plot)))
  (with-open-file (sh (file-name f-name "sh") :direction :output :if-exists :supersede :external-format :utf8)
    (format sh "#!/bin/bash~%" )
    (format sh "gnuplot ~A~%" (file-name f-name "gp")))
  (uiop:run-program (concatenate 'string "sh" " " (file-name f-name "sh")) :ignore-error-status t))

(defmethod plot ((f-name string) (term <term>) (data cons)
                 &key (preamble "set xrange [0:2.5]")
		   (plot (concatenate 'string "plot '" (file-name f-name ".data") "' u 2:1")))
  (with-open-file (gp (file-name f-name ".gp")  :direction :output :if-exists :supersede :external-format :utf8)
    (format gp "~a~%" term)
    (format gp "~a~%" (output term))
    
    (when preamble (format gp "~A~%" preamble)) 
    (when output   (format gp "~A~%" output)) 
    (when plot     (format gp "~A~%" plot)))
  (with-open-file (sh (file-name f-name "sh") :direction :output :if-exists :supersede :external-format :utf8)
    (format sh "#!/bin/bash~%" )
    (format sh "gnuplot ~A~%" (file-name f-name "gp")))
  (uiop:run-program (concatenate 'string "sh" " " (file-name f-name "sh")) :ignore-error-status t))

(defun make-plot-data-file (f-name data)
    "@b(Описание:) функция @b(make-plot-data-file) выполняет вывод
данных @b(data) в файл с именем f-name и расширением data.

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

;;;; (defmethod rotate ((obj gnuplot-vector) (angle number) (o vector)))

;(make-instance 'matrix :dimensions '(3 3))

;(make-instance 'vector 

;(defparameter *gp-v* (make-instance 'gnuplot-vector ))

;(move  *gp-v* #(-10. -10. -10.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


