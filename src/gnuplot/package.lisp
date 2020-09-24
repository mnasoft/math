;;;; /src/gnuplot/package.lisp

(defpackage #:math/gnuplot
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

