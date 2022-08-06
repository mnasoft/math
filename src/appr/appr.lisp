;;;; ./appr/package.lisp

(defpackage #:math/appr 
  (:use #:cl) ;;#:math/matr
  (:export refine-smoothing-by-points
           smooth-by-points
           approximate
           appr-table
           averaging-function-lambda
           averaging-function-defun
           )
  (:export make-approximation-defun
           make-linear-interpolation
           make-appr-linear
           make-least-squares-matrix
           make-refine-smooting
           make-approximation-lambda
           )
  (:export <appr-linear>
           appr-linear-x1
           appr-linear-a1d-func
           )
  (:export <appr-bilinear>
           appr-bilinear-a2d-func
           appr-bilinear-x1
           appr-bilinear-x2
           )
  (:export *apr-args-1*
           *apr-args-2*
           )
  (:export *apr-func-1-2*
           *apr-func-1-3*
           *apr-func-1-4*
           *apr-func-1-5*
           )
  (:export *apr-func-2-4*
           *apr-func-2-5*
           *apr-func-2-6*
           *apr-func-2-8*
           *apr-func-2-7*
           *apr-func-2-9*
           ))

(in-package #:math/appr)

;;;; appr-func-temptate.lisp

(defparameter *apr-args-1* '(x1 yy))
(defparameter *apr-args-2* '(x1 x2 yy))

(defparameter *apr-func-1-2* '((x1) (1.0) (yy)))
(defparameter *apr-func-1-3* '((x1 x1) (x1) (1.0) (yy)))
(defparameter *apr-func-1-4* '((x1 x1 x1) (x1 x1) (x1) (1.0) (yy)))
(defparameter *apr-func-1-5* '((x1 x1 x1 x1) (x1 x1 x1) (x1 x1) (x1) (1.0) (yy)))

(defparameter *apr-func-2-4* '((x1 x2) (x1) (x2) (1.0) (yy)))
(defparameter *apr-func-2-5* '((x1 x1) (x2 x2)         (x1) (x2) (1.0) (yy)))
(defparameter *apr-func-2-6* '((x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy)))
(defparameter *apr-func-2-7* '((x1 x1 x2) (x1 x2 x2) (x1 x1) (x2 x2) (x1) (x2) (1.0) (yy)))
(defparameter *apr-func-2-8* '((x1 x1 x2) (x1 x2 x2) (x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy)))
(defparameter *apr-func-2-9* '((x1 x1 x2 x2) (x1 x1 x2) (x1 x2 x2) (x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy)))

;;;; approximation.lisp

(defun appr-table (x table)
  (labels ((appr-line( XX PP0 PP1)
	     (multiple-value-bind (x p0 p1)
		 (values XX PP0 PP1)
	       (+ (second p0)
		  (/ (*
		      (- x (first p0))
		      (- (second p1) (second p0)))
		     (- (first p1) (first p0))))))
	   (appr-line-list( XX n0 n1 lst)
	     (appr-line
	      XX
	      (nth n0 lst)
	      (nth n1 lst))))
    (do ((len (length table))
	 (res nil)
	 (i 0 (1+ i)))
	((>= i len) (appr-line-list
		     x
		     (first res)
		     (second res )
		     table))
      (cond
	((and (null res) (<= x (first (nth 0 table))))
	 (setf res '(0 1)))
	((and (null res) (<= (first (nth (1- len) table)) x))
	 (setf res (list (- len 2) (- len 1))))
	((and (null res)
	      (<= (first(nth i table)) x)
	      (<= x (first(nth (1+ i) table))))
	 (setf res (list i (1+ i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Аппроксимационные полиномиальные зависимости функции нескольких переменных.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-least-squares-matrix (vv ff ex_pts)
  (let* ((m          (length ff))
	 (n          (1- m))
	 (mtr        (make-instance 'math/matr:<matrix> :dimensions  (list n m) :initial-element 0.0d0 ))
	 (mtr-lambda (make-instance 'math/matr:<matrix> :dimensions  (list n m) :initial-element nil )))
    (dotimes (i n)
      (dotimes (j m)
	(setf (math/matr:mref mtr-lambda i j)
	      (eval (list 'lambda  vv (cons '* (append (nth i ff) (nth j ff))))))))
    
    (mapc
     #'(lambda (el)
	 (dotimes (i n)
	   (dotimes (j m)
	     (setf (mref mtr i j)
		   (+ (apply (math/matr:mref mtr-lambda i j) el) (math/matr:mref mtr i j))))))
     ex_pts)
    mtr))

(defun averaging-function-body (vv ff ex_pts)
  (let ((kk
	 (cons '+
	     (mapcar
	      #'(lambda(el1 el2)
		  (cons '* (cons el1 el2)))
	      (math/matr:row (math/ls-gauss:solve-x
		    (make-least-squares-matrix vv ff ex_pts))
		   0)
	      ff)))
	(rez nil))
    (setf rez (list (reverse (cdr(reverse vv))) kk))
    rez))

(defun averaging-function-lambda (args-fuc-names func-view args-results)
  `(lambda ,@(averaging-function-body args-fuc-names func-view args-results)))

(defun averaging-function-defun (args-fuc-names func-view args-results func-name)
  `(defun ,func-name ,@(averaging-function-body args-fuc-names func-view args-results)))

(defmacro make-approximation-lambda (args-fuc-names func-view args-results)
  (averaging-function-lambda  args-fuc-names func-view args-results))

(defmacro make-approximation-defun (args-fuc-names func-view args-results func-name)
  (averaging-function-defun args-fuc-names func-view args-results func-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Разработка линейной интерполяции функции одного переменного
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-linear-interpolation (points &key (ff *apr-func-1-2*))
  (eval (averaging-function-lambda *apr-args-1* ff points)))

(defun make-linear-approximation-array (x1 a1d )
  (let ((a1-rez (make-array (mapcar #'1- (array-dimensions a1d)) :initial-element nil)))
    (loop :for r :from 0 :below (array-dimension a1-rez 0) :do
	 (setf (aref a1-rez r)
	       (make-linear-interpolation
		(list
		 (list (aref x1 r)      (aref a1d     r))
		 (list (aref x1 (1+ r)) (aref a1d (1+ r))))
		:ff *apr-func-1-2*)))
    a1-rez))

(defun index-by-value (val vect)
  (let ((rez+ 0)
	(rez- (- (array-dimension vect 0) 2)))
    (cond
      ((< (svref vect 0) (svref vect (1- (array-dimension vect 0))))
       (loop :for i :from 1 :below (1- (array-dimension vect 0)) :do
	    (when (<= (svref vect i) val ) (setf rez+ i)))
       rez+)
      ((> (svref vect 0) (svref vect (1- (array-dimension vect 0))))
       (loop :for i :from (1- (array-dimension vect 0)) :downto 1 :do
	    (when (>= val (svref vect i)) (setf rez- (1- i))))
       rez-))))

(defun approximation-linear (x1 a-x1 a-func)
  (funcall (aref a-func (index-by-value x1 a-x1)) x1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <appr-linear> ()
  ((x1       :accessor appr-linear-x1      )
   (a1d-func :accessor appr-linear-a1d-func)))

(defmethod print-object ((a-l <appr-linear>) s)
  (format s "#<appr-linear> (~A ~A)"
	  (appr-linear-x1       a-l)
	  (appr-linear-a1d-func a-l)))

(defmethod initialize-instance ((a-l <appr-linear>) &key (x1 (vector -2 -1 0 1 2)) (a1d (vector 4 1 0 1 4)))
  (setf (appr-linear-x1       a-l) x1
	(appr-linear-a1d-func a-l) (make-linear-approximation-array x1 a1d)))

(defmethod approximate ((point number) (a-l <appr-linear>))
  (approximation-linear point (appr-linear-x1 a-l) (appr-linear-a1d-func a-l)))

(defun make-appr-linear (args-resuls)
  (let ((appr-1d (make-instance '<appr-linear>
				:x1  (apply #'vector  (mapcar #'first args-resuls))
				:a1d (apply #'vector (mapcar #'second args-resuls)))))
    #'(lambda (el) (approximate el appr-1d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Линейная интерполяции функции двух переменных (билинейная интерполяция)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-bilinear-interpolation (points &key (ff *apr-func-2-4*))
  (eval (averaging-function-lambda *apr-args-2* ff points)))

(defun make-bilinear-approximation-array (a2d x1 x2)
  (when (/= 2 (array-rank a2d)) (error "In make-bilinear-approximation-array: (/= 2 (array-rank a2d))"))
  (let ((a2-rez (make-array (mapcar #'1- (array-dimensions a2d)) :initial-element nil)))
    (loop :for r :from 0 :below (array-dimension a2-rez 0) :do
	 (loop :for c :from 0 :below (array-dimension a2-rez 1) :do
	      (setf (aref a2-rez r c)
		    (make-bilinear-interpolation
		     (list
		      (list (svref x1 r)      (svref x2 c)      (aref a2d r          c))
		      (list (svref x1 r)      (svref x2 (1+ c)) (aref a2d r      (1+ c)))
		      (list (svref x1 (1+ r)) (svref x2 c)      (aref a2d (1+ r)     c))
		      (list (svref x1 (1+ r)) (svref x2 (1+ c)) (aref a2d (1+ r) (1+ c))))
		     :ff *apr-func-2-4*))))
    a2-rez))

(defun approximation-bilinear (v1 v2 a-x1 a-x2 a-func)
  (funcall (aref a-func (index-by-value v1 a-x1) (index-by-value v2 a-x2)) v1 v2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <appr-bilinear> ()
  ((x1       :accessor appr-bilinear-x1)
   (x2       :accessor appr-bilinear-x2)
   (a2d-func :accessor appr-bilinear-a2d-func)))

(defmethod print-object ((a-l <appr-bilinear>) s)
  (format s "#<appr-bilinear> (~A ~A ~A)"
	  (appr-bilinear-x1       a-l)
 	  (appr-bilinear-x2       a-l)
	  (appr-bilinear-a2d-func a-l)))

(defmethod initialize-instance ((a-l <appr-bilinear>) &key (x1 (vector -1 0 1 )) (x2 (vector 0 1 )) (a2d '#2A((1 2) (3 4) (5 6))))
  (setf (appr-bilinear-x1       a-l) x1
	(appr-bilinear-x2       a-l) x2
	(appr-bilinear-a2d-func a-l) (make-bilinear-approximation-array  a2d x1 x2)))

(defmethod approximate (point (a-l <appr-bilinear>))
  (approximation-bilinear (aref point 0) (aref point 1) (appr-bilinear-x1 a-l) (appr-bilinear-x2 a-l) (appr-bilinear-a2d-func a-l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Сглаживание методами gnuplot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric smooth-by-points (point-s base-dist-s nod-points nod-values &key weight-func))

(defgeneric refine-smoothing-by-points (nod-points nod-values base-dist-s &key weight-func delta iterations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod smooth-by-points ((x number) (dx number) (points vector) (values vector)
			     &key (weight-func #'math/smooth:gauss-smoothing))
  (assert (math/smooth:weight-func-p weight-func))
  (assert (= (length points) (length values)))
  (let ((w-summ 0.0)
	(w-z-summ 0.0)
	(w 0.0)
	(z 0.0))
    (loop :for i :from 0 :below  (array-dimension points 0) :do
	 (progn
	   (setf w (funcall weight-func (math/core:distance-relative x (aref points i) dx))
		 w-summ (+ w-summ w)
		 z (aref values i)
		 w-z-summ (+ w-z-summ (* w z )))))
    (/ w-z-summ w-summ)))

(defmethod smooth-by-points ((x vector) (dx vector) (points array) (values vector)
                             &key (weight-func #'math/smooth:gauss-smoothing))
  (assert (math/smooth:weight-func-p weight-func))
  (assert (= (array-rank points) 2))
  (assert (= (array-dimension points 0) (length values)))
  (assert (= (array-dimension points 1) (length x) (length dx)))
  (let ((w-summ 0.0)
	(w-z-summ 0.0)
	(w 0.0)
	(z 0.0))
    (loop :for i :from 0 :below  (array-dimension points 0) :do
	 (progn
	   (setf w (funcall weight-func (math/core:distance-relative x (math/matr:row i points) dx))
		 w-summ (+ w-summ w)
		 z (aref values i)
		 w-z-summ (+ w-z-summ (* w z )))))
    (/ w-z-summ w-summ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; refine-smoothing-by-points

(defmethod refine-smoothing-by-points ((nod-points vector) (nod-values vector) (base-dist-s number)
				       &key (weight-func #'math/smooth:gauss-smoothing) (delta 0.001) (iterations 10000))
  (assert (math/smooth:weight-func-p weight-func))
  (assert (= (length nod-points) (length nod-values)))
  (let ((v-iter (cl-utilities:copy-array nod-values))
	(v-rez  (cl-utilities:copy-array nod-values)))
    (labels ((start-V1 (v-rez v-iter)
	       (loop :for i :from 0 :below (length nod-values) :do
		 (setf (svref v-rez i)
		       (smooth-by-points (svref nod-points i) base-dist-s nod-points v-iter :weight-func weight-func)))
	       (math/core:summ-distance v-rez nod-values))
	     (iterate (v-rez v-iter)
	       (loop :for i :from 0 :below (length nod-values) :do
		 (setf (svref v-iter i)
		       (+ (svref v-iter i) (* 1 (- (svref nod-values i) (svref v-rez i))))))))
      (do ((i    0 (1+ i))
	   (dist (start-V1 v-rez v-iter ) (start-V1 v-rez v-iter)))
	  ((or (> i iterations) (< dist delta))
	   (progn
;;;;(format t "I=~D; DIST=~F;~%V-ITER~S;~%V-REZ=~S~%" i dist v-iter v-rez )
	     (if (< i iterations)
		 (values v-iter t   i dist v-rez nod-values)
		 (values v-iter nil i dist v-rez nod-values))))
	(iterate v-rez v-iter)))))

(defmethod refine-smoothing-by-points ((nod-points array) (nod-values vector) (base-dist-s vector)
				       &key (weight-func #'math/smooth:gauss-smoothing) (delta 0.001) (iterations 10000))
  (assert (math/smooth:weight-func-p weight-func))
  (assert (= (array-rank nod-points) 2))
  (assert (= (array-dimension nod-points 0) (length nod-values)))
  (assert (= (array-dimension nod-points 1) (length base-dist-s)))
  (let ((v-iter (cl-utilities:copy-array nod-values))
	(v-rez  (cl-utilities:copy-array nod-values)))
    (labels ((start-V1 (v-rez v-iter)
	       (loop :for i :from 0 :below (length nod-values) :do
		 (setf (svref v-rez i)
		       (smooth-by-points (math/matr:row i nod-points) base-dist-s nod-points v-iter :weight-func weight-func)))
	       (math/core:summ-distance v-rez nod-values))
	     (iterate (v-rez v-iter)
	       (loop :for i :from 0 :below (length nod-values) :do
		 (setf (svref v-iter i)
		       (+ (svref v-iter i) (* 1 (- (svref nod-values i) (svref v-rez i))))))))
      (do ((i    0 (1+ i))
	   (dist (start-V1 v-rez v-iter ) (start-V1 v-rez v-iter)))
	  ((or (> i iterations) (< dist delta))
	   (progn
	     ;;;; (format t "I=~D ; DIST=~F;~%V-ITER~S;~%V-REZ=~S~%" i dist v-iter v-rez )
	     (if (< i iterations)
		 (values v-iter t   i dist v-rez nod-values base-dist-s)
		 (values v-iter nil i dist v-rez nod-values base-dist-s))))
	(iterate v-rez v-iter)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; make-refine-smooting

(defmethod make-refine-smooting ((nod-points vector) (nod-values vector) (base-dist-s number)
				 &key (weight-func #'math/smooth:gauss-smoothing) (delta 0.001) (iterations 10000))
  (let ((new-nod-values
	  (refine-smoothing-by-points
	   nod-points nod-values base-dist-s
	   :weight-func weight-func :delta delta :iterations iterations))
	(new-nod-points  (cl-utilities:copy-array nod-points))
        (new-base-dist-s base-dist-s))
    (lambda (x)
      (smooth-by-points x
			new-base-dist-s new-nod-points new-nod-values
			:weight-func weight-func))))

(defmethod make-refine-smooting ((nod-points array) (nod-values vector) (base-dist-s vector)
				 &key (weight-func #'math/smooth:gauss-smoothing) (delta 0.001) (iterations 10000))
  (let ((new-nod-values
	  (refine-smoothing-by-points
	   nod-points nod-values base-dist-s
	   :weight-func weight-func :delta delta :iterations iterations))
	(new-nod-points  (cl-utilities:copy-array nod-points))
        (new-base-dist-s (cl-utilities:copy-array base-dist-s)))
    (lambda (x1 x2)
       (smooth-by-points (vector x1 x2) new-base-dist-s new-nod-points new-nod-values :weight-func weight-func))))
