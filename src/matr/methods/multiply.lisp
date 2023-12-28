;;;; ./src/matr/methods/multiply.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; multiply

(defmethod multiply ((a <matrix> ) (b <matrix>))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (multiply (matr-new 2 3 '(1.0 2.0 3.0
			   4.0 5.0 6.0))
	   (matr-new 3 2 '(1.0 2.0
			   3.0 4.0
			   5.0 6.0)))
 => Matr 2х2
    [ 22.0 28.0 ]
    [ 49.0 64.0 ]
 (multiply (matr-new 3 2 '(1.0 2.0
			   3.0 4.0
			   5.0 6.0))
	   (matr-new 2 3 '(1.0 2.0 3.0
			   4.0 5.0 6.0)))
 => Matr 3х3
    [ 9.0  12.0 15.0 ]
    [ 19.0 26.0 33.0 ]
    [ 29.0 40.0 51.0 ]
@end(code)
"
  (let ((a_n (rows a))
	(a_m (cols a))
	(b_n (rows b))
	(b_m (cols b))
	(c nil))
    (assert (= a_m b_n) (a_m b_n) "Запрещенные размеры матриц для перемножения: A[~A,~A] x B[~A,~A]" a_n a_m b_n b_m)
    (setf c (make-instance '<matrix> :dimensions (list a_n b_m) :initial-element 0))
    
    (do ((i 0 (1+ i))
	 (a-row nil))
	((>= i a_n))
      (setf a-row (row a i))
      (do ((j 0 (1+ j))
	   (b-col nil))
	  ((>= j b_m))
	(setf b-col (col b j))
	(setf (mref c i j)
	      (apply #'+ (mapcar #'* a-row b-col)))))
    c))

(defmethod multiply ((a number ) (b <matrix>))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (multiply 10 
           (matr-new 2 3 '(1.0 2.0 3.0
			   4.0 5.0 6.0)))
 => Matr 2х3
    [ 10.0      20.0      30.0     ]
    [ 40.0      50.0      60.0     ]
@end(code)"
  (let ((rez (make-instance '<matrix> :dimensions (dimensions b))))
    (loop :for i :from 0 :below (rows b) :do
	 (loop :for j :from 0 :below (cols b) :do
	      (setf (mref rez i j) (* a (mref b i j)))))
    rez))
