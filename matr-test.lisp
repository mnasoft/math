;;;; matr-test.lisp

(in-package #:math)

(defun print-matr (m-str m-name)
  (format t "~A ~A~%" m-str m-name ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pcl:deftest test-matrix-cols ()
  "Пример использования: (test-matrix-cols)"
  (let ((m1 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
						       (21 22 23 24 25)
						       (31 32 33 34 35)))))
    (print-matr 'm1 m1)
    (pcl:check (= (cols m1) 5))))

(pcl:deftest test-matrix-rows ()
  "Пример использования: (test-matrix-rows)"
  (let ((m1 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
						       (21 22 23 24 25)
						       (31 32 33 34 35)))))
    (print-matr 'm1 m1)
    (pcl:check (= (rows m1) 3))))

(pcl:deftest test-matrix-dimensions ()
  "Пример использования: (test-matrix-dimensions)"
  (let ((m1 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
						       (21 22 23 24 25)
						       (31 32 33 34 35)))))
    (print-matr 'm1 m1)
    (pcl:check (equal (dimensions m1) '(3 5)))))


(pcl:deftest test-matrix-get ()
  (let ((m1 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
						       (21 22 23 24 25)
						       (31 32 33 34 35)))))
    (pcl:check
      (= (mref m1 0 0 ) 11)
      (= (mref m1 0 1 ) 12)
      (= (mref m1 0 2 ) 13)
      (= (mref m1 0 3 ) 14)
      (= (mref m1 0 4 ) 15)
;;;;
      (= (mref m1 1 0 ) 21)
      (= (mref m1 1 1 ) 22)
      (= (mref m1 1 2 ) 23)
      (= (mref m1 1 3 ) 24)
      (= (mref m1 1 4 ) 25)
;;;;
      (= (mref m1 2 0 ) 31)
      (= (mref m1 2 1 ) 32)
      (= (mref m1 2 2 ) 33)
      (= (mref m1 2 3 ) 34)
      (= (mref m1 2 4 ) 35))))

(pcl:deftest test-matrix-equal ()
  (let ((m1 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
						       (21 22 23 24 25)
						       (31 32 33 34 35))))
	(m2 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
						       (21 22 23 24 25)
						       (31 32 33 34 35))))
	(m3 (make-instance 'matrix :initial-contents '((11 12 13 14 )
						       (21 22 23 24 )
						       (31 32 33 34 )))))
    (print-matr 'm1 m1)
    (print-matr 'm2 m2)
    (print-matr 'm3 m3)
    (pcl:check (equivalent m1 m2))
    (pcl:check (not (equivalent m1 (setf (mref m2 2 3) 230 ))))
    (pcl:check (not (equivalent m1 m3)))))

(pcl:deftest test-matrix-get-row ()
  "Пример использования: (test-matrix-get-row)"
  (let ((m1 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
						       (21 22 23 24 25)
						       (31 32 33 34 35)))))
    (print-matr 'm1 m1)
    (pcl:check (equal (row m1 0) '(11 12 13 14 15)))
    (pcl:check (equal (row m1 1) '(21 22 23 24 25)))
    (pcl:check (equal (row m1 2) '(31 32 33 34 35)))))

(pcl:deftest test-matrix-set-row ()
    "Пример использования: (test-matrix-set-row)"
  (let* ((m1 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
							(21 22 23 24 25)
							(31 32 33 34 35))))
	 (m2 (copy m1)))
    (setf (row m2 0) '(1 2 3 4 5)
	  (row m2 2) '(5 4 3 2 1 ))
    (print-matr 'm1 m1)
    (print-matr 'm2 m2)
    (pcl:check (equivalent (make-instance
			 'matrix
			 :initial-contents '(( 1  2  3  4  5)
					     (21 22 23 24 25)
					     ( 5  4  3  2  1)))
			m2))
    (pcl:check (not (equivalent m1 m2)))
    (pcl:check (equal (row m2 0) '(1 2 3 4 5)))))


(pcl:deftest test-matrix-get-col ()
  (let ((m1 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
						       (21 22 23 24 25)
						       (31 32 33 34 35)))))
    (print-matr 'm1 m1)
    (pcl:check (equal (col m1 0) '(11 21 31)))
    (pcl:check (equal (col m1 1) '(12 22 32)))
    (pcl:check (equal (col m1 2) '(13 23 33)))
    (pcl:check (equal (col m1 3) '(14 24 34)))
    (pcl:check (equal (col m1 4) '(15 25 35)))))

(pcl:deftest test-matrix-set-col ()
  "Пример использования: (test-matrix-set-col)"
  (let* ((m1 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
							(21 22 23 24 25)
							(31 32 33 34 35))))
	 (m2 (copy m1)))
    (setf (col m2 0) '(1 2 3)
	  (col m2 4) '(3 2 1))
    (print-matr 'm1 m1)
    (print-matr 'm2 m2)
    (pcl:check (equivalent (make-instance
			 'matrix
			 :initial-contents '(( 1 12 13 14  3)
					     ( 2 22 23 24  2)
					     ( 3 32 33 34  1)))
			m2))
    (pcl:check (not (equivalent m1 m2)))
    (pcl:check (equal (col m2 0) '(1 2 3 )))))

(pcl:deftest test-main-diagonal ()
  "Пример использования: (test-main-diagonal)"
  (let* ((m1 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
							(21 22 23 24 25)
							(31 32 33 34 35))))
	 (m2 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
							(21 22 23 24 25)
							(31 32 33 34 35)
							(41 42 43 44 45)
							(51 52 53 54 55))))
	 (m3 (make-instance 'matrix :initial-contents '((11 12 13 )
							(21 22 23 )
							(31 32 33 )
							(41 42 43 )
							(51 52 53 )))))
    (print-matr 'm1 m1)
    (print-matr 'm2 m2)
    (print-matr 'm3 m3)
    (pcl:check (equal (main-diagonal m1) '(11 22 33)))
    (pcl:check (equal (main-diagonal m2) '(11 22 33 44 55)))
    (pcl:check (equal (main-diagonal m3) '(11 22 33)))))

(pcl:deftest test-squarep ()
  "Пример использования: (test-squarep)"
  (let* ((m1 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
							(21 22 23 24 25)
							(31 32 33 34 35))))
	 (m2 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
							(21 22 23 24 25)
							(31 32 33 34 35)
							(41 42 43 44 45)
							(51 52 53 54 55))))
	 (m3 (make-instance 'matrix :initial-contents '((11 12 13 )
							(21 22 23 )
							(31 32 33 )
							(41 42 43 )
							(51 52 53 )))))
    (print-matr 'm1 m1)
    (print-matr 'm2 m2)
    (print-matr 'm3 m3)

    (pcl:check (not(squarep m1)))
    (pcl:check (squarep m2))
    (pcl:check (not(squarep m3)))))

(pcl:deftest test-main-diagonal-set ()
  "Пример использования: (test-main-diagonal-set)"
  (let* ((m1 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
							(21 22 23 24 25)
							(31 32 33 34 35))))
	 (m3 (make-instance 'matrix :initial-contents '((11 12 13 )
							(21 22 23 )
							(31 32 33 )
							(41 42 43 )
							(51 52 53 )))))
    (print-matr 'm1 m1)
    (print-matr 'm3 m3)
    (pcl:check (equivalent
	    (setf (main-diagonal m1)  '(1 2 3))
	    (make-instance 'matrix :initial-contents '(( 1 12 13 14 15)
						       (21  2 23 24 25)
						       (31 32  3 34 35)))))
    (pcl:check (equivalent
	    (make-instance 'matrix :initial-contents '(( 1 12 13 )
						       (21  2 23 )
						       (31 32  3 )
						       (41 42 43 )
						       (51 52 53 )))
	    (setf (main-diagonal m3)  '(1 2 3))))))

(pcl:deftest test-anti-diagonal ()
  "Пример использования: (test-anti-diagonal)"
  (let* ((m2 (make-instance 'matrix :initial-contents '((11 12 13 14 15)
							(21 22 23 24 25)
							(31 32 33 34 35)
							(41 42 43 44 45)
							(51 52 53 54 55)))))
    (print-matr 'm2 m2)
    (pcl:check (equal (anti-diagonal m2) '(15 24 33 42 51)))
    (pcl:check (equivalent (setf (anti-diagonal (matr-new* 3 3 '( 1 2 3 4 5 6 7 8 9))) '(11 12 13))
			(make-instance 'matrix :initial-contents '(( 1  2 11 )
								   ( 4 12  6 )
								   (13  8  9 )))))))

(pcl:deftest test-matr-mnk ()
  "Пример использования: (test-matr-mnk)"
  (let* ((m1 (make-instance 'matrix :initial-contents '(( 98.0d0    34.0d0    14.0d0    98.0d0 )
						       ( 34.0d0    14.0d0    4.0d0     34.0d0 )
						       ( 14.0d0    4.0d0     4.0d0     14.0d0 ))))
	(pts-1 '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0)))
	(m2 (make-instance 'matrix :initial-contents '(( 225.0d0   75.0d0    75.0d0    25.0d0    931.25d0 )
						       ( 75.0d0    75.0d0    25.0d0    25.0d0    606.25d0 )
						       ( 75.0d0    25.0d0    75.0d0    25.0d0    631.25d0 )
						       ( 25.0d0    25.0d0    25.0d0    25.0d0    406.25d0 ))))
	(ff-2 #'(lambda (x1 x2)
		  (+ (* x1 x1 1/2)
		     (* x2 x2 1/4)
		     (* x1 x2 )
		     (* 2 x1)
		     (* 3 x2)
		     8.0)))
	(pts-2 (let ((rez nil))
		 (loop :for x1 :from -1 :to 3 :do
		      (loop :for x2 :from -1 :to 3 :do
			   (push (list x1 x2
				       (funcall ff-2
						x1 x2)) rez )))
		 rez)))
    (pcl:check (equivalent m1 (make-least-squares-matrix '(xx yy) 
				      '((xx xx) (xx) (1.0) (yy)) 
				      pts-1 )))
    (pcl:check (equivalent m2 (make-least-squares-matrix '(x1 x2 yy) 
				      '((x1 x2)  (x1) (x2) (1.0) (yy)) 
				      pts-2)))))

(pcl:deftest test-matr-triang ()
  "Пример использования: (test-matr-triang)"
  (let ((m1(make-instance 'matrix :initial-contents '((0.0 0.0 4.0 12.0)
						      (2.0 0.0 2.0  8.0)
						      (0.0 3.0 0.0  6.0))))
	(m1-tr (make-instance 'matrix :initial-contents '(( 1.0 0.0 1.0 4.0 )
							  ( 0.0 1.0 0.0 2.0 )
    							  ( 0.0 0.0 1.0 3.0 )))))
    (print-matr 'm1 m1)
    (print-matr 'm1-tr m1-tr)
    (pcl:check (equivalent m1-tr (matr-triang* m1)))))

(pcl:deftest test-matr-obrhod ()
  "Пример использования: (test-matr-obrhod)"
  (let ((m1 (make-instance 'matrix :initial-contents '(( 1.0 0.0 1.0 4.0 )
						       ( 0.0 1.0 0.0 2.0 )
    						       ( 0.0 0.0 1.0 3.0 ))))
	(m1-obrhod (make-instance 'matrix :initial-contents '(( 1.0 2.0 3.0 )))))
    (print-matr 'm1 m1)
    (print-matr 'm1-obrhod m1-obrhod)
    (pcl:check (equivalent m1-obrhod (matr-obrhod* m1)))))

(pcl:deftest test-matr-las-gauss ()
  "Пример использования: (test-matr-las-gauss)"
  (let ((m1 (make-instance 'matrix :initial-contents '((1 2 3 14)
						       (2 1 1  7)
						       (3 0 1  2))))
	(m1-gau (make-instance 'matrix :initial-contents '(( 1/3 16/3 1 )))))
    (print-matr 'm1 m1)
    (print-matr 'm1-gau m1-gau)
    (pcl:check (equivalent (matr-las-gauss* m1) m1-gau))))

(pcl:deftest test-matr-osr ()
  "Пример использования: (test-matr-osr)"
  (pcl:check (equal
	  '((XX) (+ (* 1.0d0 XX XX) (* 0.0d0 XX) (* 0.0d0 1.0)))
	  (matr-osr-body* '(xx yy) 
			  '((xx xx) (xx) (1.0) (yy)) 
			  '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0)))))
  (pcl:check (equal
	  '(LAMBDA (XX) (+ (* 1.0d0 XX XX) (* 0.0d0 XX) (* 0.0d0 1.0)))
	  (matr-osr-lambda* '(xx yy) 
			    '((xx xx) (xx) (1.0) (yy)) 
			    '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0)))))
  (pcl:check (equal
	  '(DEFUN COOOL-FUNC (XX) (+ (* 1.0d0 XX XX) (* 0.0d0 XX) (* 0.0d0 1.0)))
	  (matr-osr-func* '(xx yy) 
			  '((xx xx) (xx) (1.0) (yy)) 
			  '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0))
			  'coool-func))))

(pcl:deftest test-matr-sum ()
  "Пример использования: (test-matr-sum)"
  (pcl:check (equivalent 
	  (matr-sum* (matr-new* 2 2 '(1 2 3 4)) (matr-new* 2 2 '(1 2 3 4)))
	  (matr-new* 2 2 '(2 4 6 8))))
  (pcl:check (equivalent 
	  (matr-sum* (matr-new* 2 2 '(1 2 3 4)) (matr-new* 2 2 '(4 3 2 1)))
	  (matr-new* 2 2 '(5 5 5 5 )))))



(pcl:deftest test-matr-mult ()
  "Пример использования: (test-matr-mult)"
  (pcl:check (equivalent
	  (matr-mult*  (matr-new* 2 3 '(1.0 2.0 3.0
					4.0 5.0 6.0))
		       (matr-new* 3 2 '(1.0 2.0
					3.0 4.0
					5.0 6.0)))
	  (matr-new* 2 2 '(22.0 28.0
			   49.0 64.0))))
  (pcl:check (equivalent
	  (matr-mult*  (matr-new* 3 2 '(1.0 2.0 3.0 4.0 5.0 6.0))
		       (matr-new* 2 3 '(1.0 2.0 3.0 4.0 5.0 6.0)))
	  (matr-new* 3 3 '( 9.0 12.0 15.0 19.0 26.0 33.0 29.0 40.0 51.0 ))))
  (pcl:check (equivalent  (matr-mult* 2 (matr-new* 3 2 '(1.0 2.0 3.0 4.0 5.0 6.0)))
		       (matr-new* 3 2 '(2.0 4.0 6.0 8.0 10.0 12.0)))))

(pcl:deftest test-matrix->2d-list ()
  "Пример использования: (test-matrix->2d-list)"
  (pcl:check (equal (matrix->2d-list (matr-new* 3 2 '(1 2 3 4 5 6))) '((1 2) (3 4) (5 6)))))

(pcl:deftest test-matrix-transpose ()
  "Пример использования: (test-matrix-transpose)"
  (let ((lst '((1 2) (3 4) (5 6))))
    (pcl:check (equal lst
		  (transpose (matrix->2d-list (transpose (make-instance 'matrix :initial-contents lst))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pcl:deftest test-matrix ()
  (pcl:combine-results
    (test-matrix-cols)
    (test-matrix-rows)
    (test-matrix-get)
    (test-matrix-equal )
    (test-matrix-get-row)
    (test-matrix-get-col)
    (test-matrix-set-row)
    (test-matrix-set-col)
    (test-squarep)
    (test-main-diagonal)
    (test-anti-diagonal)
    (test-matr-mnk)
    (test-matr-triang)
    (test-matr-obrhod)
    (test-matr-las-gauss)
;    (test-matr-matr-osr)
    (test-matr-sum)
    (test-matr-mult)
    (test-matrix->2d-list)
    (test-matrix-transpose)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;(test-matrix) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(make-instance 'grid:matrix :dimensions '(3 3) :element-type 'double  :initial-element 1.0d0)

;;(lu-solve-extmatr '((1 2 3 14) (2 1 1  7) (3 0 1  2)))