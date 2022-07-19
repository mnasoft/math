;;;; math.lisp

(defpackage #:math/core
  (:use #:cl)
  (:export summ-distance 
	   distance               
	   exclude-nil-from-list  
	   depth-sphere-along-cone
	   distance-relative
	   square)
  (:export split-range
	   split-range-by-func )
  (:export mref
	   col
	   row
	   
	   cols	   
	   rows
	   dimensions

	   add
	   multiply
	   
	   squarep
	   
	   matr-name-*
	     
	   main-diagonal
	   anti-diagonal

	   equivalent
	   copy

	   transpose
	   )
  (:export round-to-significant-digits
           +significant-digits+))

(in-package :math/core)

(defun square (x)
  (* x x))

(defun exclude-nil-from-list (lst)
  (let ((res nil))
    (dolist (el lst (reverse res) )
      (when el (push el res )))))

(defun e-value (n)
   (let ((rez 1)
	(nf 1))
    (dotimes (i n rez)
      (setf nf  (/ nf (1+ i))
	    rez (+ rez nf)))))

(defun split-range (from to steps)
 (loop :for i :from 0 :to steps
	:collect (coerce (+ from (* (/ i steps ) (- to from))) 'float)))

(defun split-range-by-func (from to steps &key
					    (func #'(lambda (x) (log x 10)))
					    (anti-func #'(lambda (x) (expt 10 x))))
 (mapcar
   #'(lambda (el)(funcall anti-func el))
   (split-range (funcall func from) (funcall func to) steps)))

(defun depth-sphere-along-cone (r alpha)
   (let ((betta (- pi (/ alpha 2))))
    (- r (* r (tan (/ betta  2))))))

(defparameter +significant-digits+ 4)

(defun round-to-significant-digits (val &optional (significant-digits +significant-digits+) (base-val val))
   (labels ((find-divisor (val)
	     "@b(Описание:) функция @b(find-divisor)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (find-divisor 10.964739714723287d0)
@end(code)
"
	     (do ((divisor 1))
		 ((<= 1 (abs (* val divisor)) 10) divisor)
	       (cond
		 ((< (abs (* val divisor)) 1) (setf divisor (* divisor 10)))
		 ((< 1 (abs (* val divisor))) (setf divisor (/ divisor 10))))))

	   (my-round (val &optional (sb-kernel::divisor 1))
	     "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (my-round 10.964739714723287d0 1/100) => 10.96
@end(code)
"
	     (coerce (* (round val sb-kernel::divisor)
			sb-kernel::divisor)
		     'single-float)))
    (my-round val
              (/ (expt 10 (* -1 (- significant-digits 1)))
		 (find-divisor base-val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; /src/core/generic.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric distance (x1 x2))

(defgeneric distance-relative (x xi dx))

(defgeneric summ-distance (x1 x2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; /src/core/generic-matr.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric matr-name-* (matrix))

(defgeneric dimensions (matrix))

(defgeneric rows (matrix))

(defgeneric cols (matrix))

(defgeneric equivalent (matrix-1 matrix-2 &key test))

(defgeneric row (matrix row))

(defgeneric (setf row) (values matrix row)
    (:documentation "@b(Описание:) обобщенная_функция @b((setf row))
заменяет строку @b(row) матрицы @b(matrix) элементами, находящимися в списке @b(values)."))

(defgeneric col (matrix col))

(defgeneric (setf col) (values matrix col)
    (:documentation "@b(Описание:) обобщенная_функция @b((setf col))
заменяет столбец @b(col) матрицы @b(matrix) элементами, находящимися в списке @b(values)."))

(defgeneric main-diagonal (matrix))

(export 'main-diagonal)

(defgeneric (setf main-diagonal) (elements matrix)
  (:documentation
"@b(Описание:) обобщенная_функция @b((setf main-diagonal)) устанавливет 
новые значения элементам матрицы @b(matrix), находящимся на главной диагонали.

 Элементы @b(elements) устанавливаются в порядке возрастания строк."))

(defgeneric anti-diagonal (matrix))

(defgeneric (setf anti-diagonal) (elements matrix)
  (:documentation
"@b(Описание:) обобщенная_функция @b((setf anti-diagonal)) устанавливет 
новые значения элементам матрицы @b(matrix), на побочной диагонали матрицы.

 Элементы @b(elements) устанавливаются в порядке возрастания строк."))

(defgeneric squarep (matrix))

(defgeneric mref (matrix row col))

(defgeneric (setf mref) (value matrix row col)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(mref) устанавливает
значение @b(value) элементу матрицы, находящемуся в 
строке @b(row) и столбце @b(col) ."))

(defgeneric copy (obj))

(defgeneric add (a b))

(defgeneric multiply (a b))

(defgeneric transpose (matrix))
