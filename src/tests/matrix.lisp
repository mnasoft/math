;;;; 

(in-package #:math/tests)

(defun make-<matrix>-int (rows cols)
  "@b(Описание:) функция @b(make-<matrix>-int) возвращает матрицу 
с количеством строк rows=1..9 и столбцов cols=1..9 и 
ининциализирует элементы целыми значениями.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-<matrix>-int 2 3)
 => Matr 2х3
 [ 11 12 13 ]
 [ 21 22 23 ]
@end(code)"
  (assert (< 0 rows 10))
  (assert (< 0 cols 10))
  (let ((matr (make-instance  'math/matr:<matrix> :initial-element 0 :dimensions (list rows cols))))
    (loop :for i :from 0 :below rows :do
      (loop :for j :from 0 :below cols :do
	(setf (math/matr:mref matr i j) (+ 11 (* 10 i) j))))
    matr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite matrix-tests
  :description "Мастер-набор всех тестов проекта math."
  :in all)

(in-suite matrix-tests)

(def-fixture fix-<matrix>-m1-3x5 ()
    (let ((m1-3x5 (make-<matrix>-int 3 5)))
      (&body)))

(def-fixture fix-<matrix>-m2-3x5 ()
    (let ((m2-3x5 (make-<matrix>-int 3 5)))
      (&body)))

(def-fixture fix-a1-a2-a3-a4-2x2 ()
  (let ((a1 (make-array '(2 2) :initial-contents  '((10.00000 10.0000)
						    (11.00000 11.0000))))
	(a2 (make-array '(2 2) :initial-contents  '((10.00000 10.00000)
						    (11.000000 11.000001))))
	(a3 (make-array '(2 2) :initial-contents  '(( 9.999999  9.999999)
						    (10.999999 10.999999))))
	(a4 (make-array '(2 2) :initial-contents  '((10.00100 10.0010)
						    (11.00100 11.0010)))))
    (&body)))

(def-test matrix-size-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-<matrix>-m1-3x5 ()
    (is-true (= (math/matr:cols m1-3x5) 5))
    (is-true (= (math/matr:rows m1-3x5) 3))
    (is-true (equal (math/matr:dimensions m1-3x5) '(3 5)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test matrix-mref-test ()
  "Проверка доступа к элементам матрицы."
  (with-fixture fix-<matrix>-m1-3x5 ()
    (is-true (= (math/matr:mref m1-3x5 0 0 ) 11))
    (is-true (= (math/matr:mref m1-3x5 0 1 ) 12))
    (is-true (= (math/matr:mref m1-3x5 0 2 ) 13))
    (is-true (= (math/matr:mref m1-3x5 0 3 ) 14))
    (is-true (= (math/matr:mref m1-3x5 0 4 ) 15))
;;;;
    (is-true (= (math/matr:mref m1-3x5 1 0 ) 21))
    (is-true (= (math/matr:mref m1-3x5 1 1 ) 22))
    (is-true (= (math/matr:mref m1-3x5 1 2 ) 23))
    (is-true (= (math/matr:mref m1-3x5 1 3 ) 24))
    (is-true (= (math/matr:mref m1-3x5 1 4 ) 25))
;;;;
    (is-true (= (math/matr:mref m1-3x5 2 0 ) 31))
    (is-true (= (math/matr:mref m1-3x5 2 1 ) 32))
    (is-true (= (math/matr:mref m1-3x5 2 2 ) 33))
    (is-true (= (math/matr:mref m1-3x5 2 3 ) 34))
    (is-true (= (math/matr:mref m1-3x5 2 4 ) 35))))

(def-test matrix-mref-set-test ()
  "Проверка доступа к элементам матрицы."
  (with-fixture fix-<matrix>-m1-3x5 ()
    (setf (math/matr:mref m1-3x5 0 0) -11
	  (math/matr:mref m1-3x5 0 1) -12
	  (math/matr:mref m1-3x5 0 2) -13
	  (math/matr:mref m1-3x5 0 3) -14
	  (math/matr:mref m1-3x5 0 4) -15
;;;;
	  (math/matr:mref m1-3x5 1 0) -21
	  (math/matr:mref m1-3x5 1 1) -22
	  (math/matr:mref m1-3x5 1 2) -23
	  (math/matr:mref m1-3x5 1 3) -24
	  (math/matr:mref m1-3x5 1 4) -25
;;;;
	  (math/matr:mref m1-3x5 2 0) -31
	  (math/matr:mref m1-3x5 2 1) -32
	  (math/matr:mref m1-3x5 2 2) -33
	  (math/matr:mref m1-3x5 2 3) -34
	  (math/matr:mref m1-3x5 2 4) -35)

    
    (is-true (= (math/matr:mref m1-3x5 0 0 ) -11))
    (is-true (= (math/matr:mref m1-3x5 0 1 ) -12))
    (is-true (= (math/matr:mref m1-3x5 0 2 ) -13))
    (is-true (= (math/matr:mref m1-3x5 0 3 ) -14))
    (is-true (= (math/matr:mref m1-3x5 0 4 ) -15))
;;;;
    (is-true (= (math/matr:mref m1-3x5 1 0 ) -21))
    (is-true (= (math/matr:mref m1-3x5 1 1 ) -22))
    (is-true (= (math/matr:mref m1-3x5 1 2 ) -23))
    (is-true (= (math/matr:mref m1-3x5 1 3 ) -24))
    (is-true (= (math/matr:mref m1-3x5 1 4 ) -25))
;;;;
    (is-true (= (math/matr:mref m1-3x5 2 0 ) -31))
    (is-true (= (math/matr:mref m1-3x5 2 1 ) -32))
    (is-true (= (math/matr:mref m1-3x5 2 2 ) -33))
    (is-true (= (math/matr:mref m1-3x5 2 3 ) -34))
    (is-true (= (math/matr:mref m1-3x5 2 4 ) -35))))

(def-test matrix-equivalent-test ()
  "Проверка эквивалентности матриц."
  (with-fixture fix-<matrix>-m1-3x5 ()
    (with-fixture fix-<matrix>-m2-3x5 ()
      (with-fixture fix-a1-a2-a3-a4-2x2 ()
	(is-true  (math/matr:equivalent m1-3x5 m2-3x5))
	(is-true  (math/matr:equivalent m2-3x5 m1-3x5))
	(is-true  (math/matr:equivalent a1 a2))
	(is-false (math/matr:equivalent a1 a2 :test #'equal))
	(is-true  (math/matr:equivalent a1 a3))
	(is-false (math/matr:equivalent a1 a4))
        (is-true  (math/matr:equivalent
		   (make-instance 'math/matr:<matrix> :data a1)
		   (make-instance 'math/matr:<matrix> :data a2)))
	(is-true  (math/matr:equivalent
		   (make-instance 'math/matr:<matrix> :data a1)
		   (make-instance 'math/matr:<matrix> :data a3)))
        (is-false (math/matr:equivalent
		   (make-instance 'math/matr:<matrix> :data a1)
		   (make-instance 'math/matr:<matrix> :data a4)))))))


(def-test matrix-get-row ()
  "Проверка доступа к строкам."
  (with-fixture fix-<matrix>-m1-3x5 ()
    (is-true (equal (math/matr:row m1-3x5 0) '(11 12 13 14 15)))
    (is-true (equal (math/matr:row m1-3x5 1) '(21 22 23 24 25)))
    (is-true (equal (math/matr:row m1-3x5 2) '(31 32 33 34 35))))
  )

(def-test matrix-set-row-tests ()
  "Пример использования: (test-matrix-set-row)"
  (with-fixture fix-<matrix>-m1-3x5 ()
    (let ((m-cp (math/matr:copy m1-3x5)))
      (setf (math/matr:row m-cp 0) '(1 2 3 4 5)
	    (math/matr:row m-cp 2) '(5 4 3 2 1))
      (is-true (math/matr:equivalent
		(make-instance
		 'math/matr:<matrix>
		 :initial-contents '(( 1  2  3  4  5)
				     (21 22 23 24 25)
				     ( 5  4  3  2  1)))
		m-cp))
      (is-false (math/matr:equivalent m1-3x5 m-cp))
      (is-true  (equal (math/matr:row m-cp 0) '(1 2 3 4 5)))
      (is-true  (equal (math/matr:row m-cp 2) '(5 4 3 2 1))))))

(def-test matrix-get-col-test ()
  "Проверка доступа к столбцам."
  (with-fixture fix-<matrix>-m1-3x5 ()
    (is-true (equal (math/matr:col m1-3x5 0) '(11 21 31)))
    (is-true (equal (math/matr:col m1-3x5 1) '(12 22 32)))
    (is-true (equal (math/matr:col m1-3x5 2) '(13 23 33)))
    (is-true (equal (math/matr:col m1-3x5 3) '(14 24 34)))
    (is-true (equal (math/matr:col m1-3x5 4) '(15 25 35)))))

(def-test matrix-set-col-test ()
  "Пример использования"
  (with-fixture fix-<matrix>-m1-3x5 ()
    (let ((m-cp (math/matr:copy m1-3x5)))
      (setf (math/matr:col m-cp 0) '(1 2 3)
	    (math/matr:col m-cp 4) '(3 2 1))
      (is-true (math/matr:equivalent (make-instance
				 'math/matr:<matrix>
				 :initial-contents '(( 1 12 13 14  3)
						     ( 2 22 23 24  2)
						     ( 3 32 33 34  1)))
				m-cp))
      (is-false (math/matr:equivalent m1-3x5 m-cp))
      (is-true  (equal (math/matr:col m-cp 0) '(1 2 3 ))))))


(def-test matrix-main-diagonal-test ()
  "Проверка извлечения элементов главной диагонали."
  (let ((m-3x5 (make-<matrix>-int 3 5))
	(m-5x5 (make-<matrix>-int 5 5))
	(m-5x3 (make-<matrix>-int 5 3)))
    (is-true (equal (math/matr:main-diagonal m-3x5) '(11 22 33)))
    (is-true (equal (math/matr:main-diagonal m-5x5) '(11 22 33 44 55)))
    (is-true (equal (math/matr:main-diagonal m-5x3) '(11 22 33)))))

(def-test matrix-squarep-test ()
  "Проверка матрицы на квадратность."
    (is-true  (math/matr:squarep (make-<matrix>-int 1 1)))
    (is-true  (math/matr:squarep (make-<matrix>-int 2 2)))
    (is-false (math/matr:squarep (make-<matrix>-int 1 2)))
    (is-true  (math/matr:squarep (make-<matrix>-int 3 3)))
    (is-false (math/matr:squarep (make-<matrix>-int 3 5)))
    (is-false (math/matr:squarep (make-<matrix>-int 5 3))))


(def-test matrix-main-diagonal-set-test ()
  "Пример использования: (test-main-diagonal-set)"
  (let ((m-3x5 (make-<matrix>-int 3 5))
	(m-5x3 (make-<matrix>-int 5 3))
 	(m-5x5 (make-<matrix>-int 5 5))
	(m-3x5-d (make-instance 'math/matr:<matrix>
				:initial-contents '(( 1 12 13 14 15)
						    (21  2 23 24 25)
						    (31 32  3 34 35))))
	(m-5x3-d (make-instance 'math/matr:<matrix>
				:initial-contents '(( 1 12 13 )
						    (21  2 23 )
						    (31 32  3 )
						    (41 42 43 )
						    (51 52 53 ))))
	(m-5x5-d (make-instance 'math/matr:<matrix>
				:initial-contents '(( 1 12 13 14 15)
						    (21  2 23 24 25)
						    (31 32  3 34 35)
						    (41 42 43  4 45)
						    (51 52 53 54  5)))))
    (setf (math/matr:main-diagonal m-3x5) '(1 2 3)
	  (math/matr:main-diagonal m-5x3) '(1 2 3)
	  (math/matr:main-diagonal m-5x5) '(1 2 3 4 5))
    
    
    (is-true (math/matr:equivalent m-3x5 m-3x5-d))
    (is-true (math/matr:equivalent m-5x3 m-5x3-d))
    (is-true (math/matr:equivalent m-5x5 m-5x5-d))))

(def-test matrix-anti-diagonal-test ()
  "Проверка операций с побочной диагональю."
  (let* ((m-5x5 (make-<matrix>-int 5 5))
	 (m-5x5-d (make-instance 'math/matr:<matrix>
				 :initial-contents '((11 12 13 14  1)
						     (21 22 23  2 25)
						     (31 32  3 34 35)
						     (41  4 43 44 45)
						     ( 5 52 53 54 55)))))
    (is-true (equal (math/matr:anti-diagonal m-5x5) '(15 24 33 42 51)))
    (is-true (setf  (math/matr:anti-diagonal m-5x5) '(1 2 3 4 5)))
    (is-true (math/matr:equivalent m-5x5 m-5x5-d))))

(def-test matrix-add-tests ()
  "Проверка операций сложения."
  (is-true (math/matr:equivalent 
	    (math/matr:add (math/matr:matr-new 2 2 '(1 2 3 4)) (math/matr:matr-new 2 2 '(1 2 3 4)))
	    (math/matr:matr-new 2 2 '(2 4 6 8))))
  (is-true (math/matr:equivalent 
	    (math/matr:add (math/matr:matr-new 2 2 '(1 2 3 4)) (math/matr:matr-new 2 2 '(4 3 2 1)))
	    (math/matr:matr-new 2 2 '(5 5 5 5 )))))

(def-test matrix-multiply-tests ()
  "Проверка опрераций умножения."
  (is-true (math/matr:equivalent
	    (math/matr:multiply  (math/matr:matr-new 2 3 '(1.0 2.0 3.0
						   4.0 5.0 6.0))
			      (math/matr:matr-new 3 2 '(1.0 2.0
						   3.0 4.0
						   5.0 6.0)))
	    (math/matr:matr-new 2 2 '(22.0 28.0
				 49.0 64.0))))
  (is-true (math/matr:equivalent
	    (math/matr:multiply  (math/matr:matr-new 3 2 '(1.0 2.0 3.0 4.0 5.0 6.0))
			      (math/matr:matr-new 2 3 '(1.0 2.0 3.0 4.0 5.0 6.0)))
	    (math/matr:matr-new 3 3 '( 9.0 12.0 15.0 19.0 26.0 33.0 29.0 40.0 51.0 ))))
  (is-true (math/matr:equivalent  (math/matr:multiply 2 (math/matr:matr-new 3 2 '(1.0 2.0 3.0 4.0 5.0 6.0)))
			     (math/matr:matr-new 3 2 '(2.0 4.0 6.0 8.0 10.0 12.0)))))

(def-test matrix-transpose-tests ()
  "Проверка операции транспонирования."
  (is-true  (math/matr:transpose
	     (math/matr:matr-new 2 3 '(11 12 13
				  21 22 23)))
	    (math/matr:matr-new 3 2 '(11 21
				 12 22
				 13 23)))
  (is-true  (equal (math/matr:transpose '((1 2)
				     (3 4)
				     (5 6)))
		   '((1 3 5)
		     (2 4 6)))))

(def-test matrix->2d-list-tests ()
  "Пример использования: (test-matrix->2d-list)"
  (is-true
   (equal
    (math/matr:matrix->2d-list
     (math/matr:matr-new 3 2 '(1 2 3 4 5 6)))
    '((1 2) (3 4) (5 6)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
