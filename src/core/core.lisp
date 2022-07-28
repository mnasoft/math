;;;; ./src/core/core.lisp

(defpackage #:math/core
  (:use #:cl)
  (:export norma
           *semi-equal-relativ*
           *semi-equal-zero*
           semi-equal
           )
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

(defgeneric distance-relative (x0 x1 dx))

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

(defgeneric (setf row) (values matrix row))

(defgeneric col (matrix col))

(defgeneric (setf col) (values matrix col))

(defgeneric main-diagonal (matrix))

(defgeneric (setf main-diagonal) (elements matrix))

(defgeneric anti-diagonal (matrix))

(defgeneric (setf anti-diagonal) (elements matrix))

(defgeneric squarep (matrix))

(defgeneric mref (matrix row col))

(defgeneric (setf mref) (value matrix row col))

(defgeneric copy (obj))

(defgeneric add (a b))

(defgeneric multiply (a b))

(defgeneric transpose (matrix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *semi-equal-relativ* 1e-6
  "@b(Описание:) переменная @b(*semi-equal-relativ*) определяет
относительную величину, на которую могут отличаться значения 
считающиеся равными при срвнении из с помощью функции @b(semi-equal).
")

(defparameter *semi-equal-zero*    1e-6
    "@b(Описание:) переменная @b(*semi-equal-zero*) определяет
абсолютную величину, на которую могут отличаться значения 
считающиеся равными при срвнении из с помощью функции @b(semi-equal).
")

(defgeneric norma (x))

(defmethod norma ((x real))
  (abs x))

(defmethod norma ((x number))
  (sqrt (+ (* (realpart x) (realpart x))
           (* (imagpart x) (imagpart x)))))

(defmethod norma ((x cons))
  (let ((n (loop :for i :in x
                 :collect (* (norma i)))))
    (/ (apply #'+ n) (length n))))

(defmethod norma ((x vector))
  (let ((n (loop :for i :across x
                 :collect (* (norma i)))))
    (/ (apply #'+ n) (length n))))

(defgeneric semi-equal (x y &key tolerance) )

(defmethod semi-equal ((x number) (y number)
		       &key
                         (tolerance (+ *semi-equal-zero*
                                       (* *semi-equal-relativ*
                                          (norma (list (norma x) (norma y)))))))
  "@b(Описание:) функция @b(semi-equal) возвращает T, если 
расстояние между значениями меньше tolerance. При этом 
имеется в виду, что значения примерно равны.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (semi-equal 1.0 1.000001) T
 (semi-equal 1.0 1.00001)  nil
@end(code)
"
  (< (distance x y) tolerance))

(defmethod semi-equal ((x cons) (y cons)
                       &key
                         (tolerance (+ *semi-equal-zero*
                                       (* *semi-equal-relativ*
                                          (norma (list (norma x)
                                                       (norma y)))))))
  (when (and (= (length x) (length y))
             (< (distance x y) tolerance))
    t))

(defmethod semi-equal ((x vector) (y vector)
                       &key
                         (tolerance (+ *semi-equal-zero*
                                       (* *semi-equal-relativ*
                                          (norma (list (norma x)
                                                       (norma y)))))))
  (when (and (= (length x) (length y))
             (< (distance x y) tolerance))
    t))

(defmethod semi-equal ((x1 vector) (x2 cons)
                       &key
                         (tolerance (+ *semi-equal-zero*
                                       (* *semi-equal-relativ*
                                          (norma (list (norma x1)
                                                       (norma x2)))))))
  (semi-equal x1 (coerce x2 'vector) :tolerance tolerance))

(defmethod semi-equal ((x1 cons) (x2 vector) 
                       &key
                         (tolerance (+ *semi-equal-zero*
                                       (* *semi-equal-relativ*
                                          (norma (list (norma x1)
                                                       (norma x2)))))))
  (semi-equal (coerce x1 'vector) x2 :tolerance tolerance))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; /src/core/method.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; distance

(defmethod distance ((x1 real) (x2 real) )
  (let ((rez (- x1 x2)))
    (abs rez)))

(defmethod distance ((x1 number) (x2 number))
  (sqrt (+ (square (- (realpart x1) (realpart x2)))
           (square (- (imagpart x1) (imagpart x2))))))

(defmethod distance ((x1-lst cons) (x2-lst cons))
  (assert (= (length x1-lst) (length x1-lst)))
  (sqrt (apply #'+ (mapcar
		    #'(lambda (x1 x2)
			  (square (distance x1 x2)))
		    x1-lst x2-lst))))

(defmethod distance ((x1 vector) (x2 vector))
  (assert (= (length x1) (length x2)))
  (sqrt (loop :for i :from 0 :below (array-dimension x1 0)
	      :summing
              (square (distance (svref x1 i) (svref x2 i))))))

(defmethod distance ((x1 vector) (x2 cons))
  (distance x1 (coerce x2 'vector)))

(defmethod distance ((x1 cons) (x2 vector))
  (distance (coerce x1 'vector) x2))


;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; distance-relative

(defmethod distance-relative ((x number) (xi number) (dx number))
  (let ((rez (/ (- x xi) dx)))
    (sqrt (square rez))))

(defmethod distance-relative ((x-lst cons) (xi-lst cons) (dx-lst cons))
  (assert (apply #'= (mapcar #'length `(,x-lst ,xi-lst ,dx-lst))))
  (sqrt (apply #'+ (mapcar
		    #'(lambda (x xi dx)
			(let ((rez (/ (- x xi) dx)))
			  (square rez)))
		    x-lst xi-lst dx-lst))))

(defmethod distance-relative ((x vector) (xi vector) (dx vector))
  (assert (apply #'= (mapcar #'length `(,x ,xi ,dx))))
  (sqrt (apply #'+ (loop :for i :from 0 :below (array-dimension x 0)
			 :collect
			 (let ((rez (/ (- (svref x i) (svref xi i)) (svref dx i))))
			   (square rez))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; summ-distance

(defmethod summ-distance ((x1 vector) (x2 vector))
  (assert (= (length x1) (length x2)))
  (apply #'+ (loop :for i :from 0 :below (length x1)
		   :collect (abs (- (svref x1 i) (svref x2 i))))))

(defmethod summ-distance ((x1 cons) (x2 cons))
  (assert (= (length x1) (length x2)))
  (apply #'+
	 (mapcar
	  #'(lambda (el1 el2)
	      (abs (- el1 el2)))
	  x1 x2)))
