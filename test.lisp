;;;; matr.lisp

(in-package #:math)

(require :gsll)

(defun lu-solve (matrix vector)
  "Solve the linear equation using LU with the supplied matrix and
   a right-hand side vector which is the reciprocal of one more than
   the index."
      (multiple-value-bind (upper permutation signum) (gsl:LU-decomposition (grid:copy matrix))
      (declare (ignore signum))
      (let ((initial-solution (gsl:LU-solve upper vector permutation T)))
	(gsl:LU-refine matrix upper permutation vector initial-solution))))

(defun lu-solve-extmatr (matrix-vector)
  ""
  (let ((matrix (grid:make-foreign-array
		 'double-float
		 :initial-contents
		 (list-matr-detach-last-col matrix-vector)))
	(vector (grid:make-foreign-array
		 'double-float
		 :initial-contents
		 (list-matr-get-last-col    matrix-vector))))
    (lu-solve matrix vector)))


(defparameter *data*
  '((  0	0	8)
    (1	0	26)
    (0	1	17)
    (-1	-1	14)
    (-1	1	18)
    (0	1	17)
    (3	3	218)
    (3	2	186)
    (2	3	137)
    (-2	-3	71)
    (-1.9	-2.8	62.6)
    (-1.8	-2.6	54.8)
    (-1.7	-2.4	47.6)
    (-1.6	-2.2	41)
    (-1.5	-2	35)
    (-1.4	-1.8	29.6)
    (-1.3	-1.6	24.8)
    (-1.2	-1.4	20.6)
    (-1.1	-1.2	17)
    (-0.999999999999999	-1	14)
    (-0.899999999999999	-0.8	11.6)
    (-0.799999999999999	-0.6	9.79999999999999)
    (-0.699999999999999	-0.4	8.59999999999999)
    (-0.599999999999999	-0.2	8)
    (-0.499999999999999	0	8)
    (-0.399999999999999	0.2	8.6)
    (-0.299999999999999	0.4	9.8)
    (-0.199999999999999	0.6	11.6)
    (-0.099999999999999	0.8	14)))

(fff 2.4 4.93)



(lu-solve-extmatr 
 (matr->2d-list
  (matr-mnk '(x1 x2 yy) 
	    '((x1 x1 )(x2 x2)  (x1 x2)  (x1) (x2) (1.0) (yy))
	    *data*)))
#m(12.000000779935279d0 1.999999797660067d0 4.999999737704854d0 5.999998441971560d0 7.000000899416466d0 7.999999274872341d0)

(matr-osr-func '(x1 x2 yy) 
	    '((x1 x1 )(x2 x2)  (x1 x2)  (x1) (x2) (1.0) (yy))
	    *data*
	    'fff)

(defun list-matr-detach-last-col (2d-list)
  (mapcar
   #'(lambda(el) (reverse (cdr (reverse el))))
   2d-list))

(defun list-matr-get-last-col (2d-list)
  (mapcar
   #'(lambda(el) (car (last el)))
   2d-list))


(matr-osr-func '(xx yy) 
	       '((xx xx xx) (xx xx) (xx) (1.0) (yy))
	       '((-2.0 4.0) (-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0))
	       'coool-func)

(let ((x (append '(  -2000. -50.0) (make-random-value-list 100.0 :std-deviation 1.0 :n 8 ))))
  (values
   (clean-flagrant-error x)
   (clean-min-flagrant-error x)
   (clean-max-flagrant-error x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(matr-osr-func '(xx yy) 
	       '((xx xx xx) (xx xx) (xx) (1.0) (yy))
	       '((-2.0 4.0) (-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0))
	       'coool-func)

ΔPВК, кПа	ΔPДТ1, МПа	ΔPДТ2, МПа	ΔPВ2, кПа	GДТ, кг/ч	α, град	К	tдт, °С
-	0,646	-	-	34,6	92	3	27,6
-	0,646	-	10,0	34,6	96	2	27,6
-	0,646	-	20,0	34,6	97	1, 2	27,6
-	0,646	-	30,0	34,6	97	1а	27,6
5,0	0,646	-	33,1	34,6	82	1а	27,7
7,0	0,646	-	33,1	34,6	77	1а	27,7
10,0	0,646	-	33,1	34,6	77	1а	27,7
-	0,894	0,068	-	123	95	3	29
3,0	0,894	0,068	-	123	82	4	28
5,0	0,894	0,068	-	123	77	4	28,5
7,0	0,894	0,068	-	122	74	4	29
10,0	0,894	0,068	-	125,6	69	3, 4	29,5
15,0	0,894	0,068	-	125,6	63	1, 3	30
25,0	0,894	0,068	-	125,6	57	1	31
-	1,088	0,136	-	143,5	94	2, 3	29
3,0	1,088	0,136	-	147,6	87	3, 4	33,1
5,0	1,088	0,136	-	144,7	81	3, 4	32
10,0	1,088	0,136	-	144,7	74	1, 3	31,3
15,0	1,088	0,136	-	144,7	70	1	31
25,0	1,088	0,136	-	144	61	1	30,6
-	1,424	0,413	-	219,5	94	2	29
5,0	1,269	0,275		219,5	88	3	28,5
10,0	1,269	0,275	-	219,5	82	1, 2	28,2
15,0	1,269	0,275	-	219,5	80	1	28,1
25,0	1,269	0,275	-	219,5	73	1	28
30,0	1,269	0,275	-	219,5	69	1	28
35,0	1,269	0,275	-	219,5	69	1	27,9
-	1,594	0,589	-	251,5	94	2	27,9
10,0	1,594	0,589	-	251,5	84	1, 2	28
20,0	1,594	0,589	-	251,5	81	1	28
30,0	1,594	0,589	-	251,5	76	1	28,1
45,0	1,594	0,589	-	251,5	70	1	28,1
-	1,910	0,890	-	302	95	2	28,2
10,0	1,910	0,890	-	302	86	2	28,3
20,0	1,910	0,890	-	303	83	1, 2	28,3
30,0	1,910	0,890	-	303	79	1	28,3
50,0	1,910	0,890	-	303	72	1	28,4
-	2,300	1,360	-	367,5	94	2	28,5
10,0	2,300	1,360	-	367,5	90	1, 2	28,6
20,0	2,300	1,360	-	367,5	84	1, 2	28,7
30,0	2,300	1,360	-	367,5	80	1, 2	28,8
45,0	2,300	1,360	-	367,5	75	1а, 2	28,8
60,0	2,300	1,360	-	367,5	72	1а, 2	28,9



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(list-matr-union *l-m-test*)

(mapcar #'append *l-m-test*)

(defparameter *l-m-test*
  '(( 1  2  3  4  5  6  7)
    ( 8  9 10 11 12 13 14)
    (15 16 17 18 19 20 nil)
    (22 23 24 25 26 27 nil)))

(list-matr-rows *l-m-test*)

(list-matr-cols *l-m-test*)

(list-matr-row 4 *l-m-test*)

(list-vector-print (list-matr-col 6 *l-m-test*))

(list-matr-averange-value *l-m-test*)

(list-matr-averange-not-nil-value  *l-m-test*)

(list-matr-averange-col-not-nil-value *l-m-test*)

(list-matr-transpose *l-m-test*)


(progn 
  (list-matr-print  *l-m-test* :fmt "~4,1f" )
  (list-vector-print (list-matr-averange-col-not-nil-value *l-m-test*) :fmt "~4,1f" ))

(progn
  (list-matr-print *l-m-test*)
  (list-matr-print (list-matr-append-col '("q" "s" nil "q" "u" "r") *l-m-test*)))

(progn
  (list-matr-print *l-m-test*)
  (list-matr-print (list-matr-append-row '("q" "s" nil "q" "u" "r") *l-m-test*)))

(progn
  (list-matr-print *l-m-test*)
  (list-matr-print (list-matr-prepend-col '("q" "s" nil "q" "u" "r") *l-m-test*)))

(progn
  (list-matr-print *l-m-test*)
  (list-matr-print (list-matr-prepend-row '("q" "s" nil "q" "u" "r") *l-m-test*)))

(exclude-nil-from-list '( 10 nil 30 nil 20 15))


(list-matr-print (list-matr-make 2 3 '(1 2 3 4 5 6)))
