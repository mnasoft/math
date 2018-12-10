;;;; matr-class.lisp

(in-package #:math)

(defclass matrix ()
  ((data :accessor matrix-data :initform nil :initarg :data)))

(defmethod matr-name-* ((mm matrix)) "Matr")

(defmethod print-object ((mm matrix) s)
  (format s "~A " (matr-name-* mm))
  (when (and (matrix-data mm) (arrayp (matrix-data mm)))
    (format s "~{~A~^х~}" (array-dimensions (matrix-data mm)))
    (loop :for i :from 0 :below (array-dimension (matrix-data mm) 0)
       :do
	 (format s "~%[")
	 (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1)
	    :do (format s " ~8A " (aref (matrix-data mm) i j)))
	 (format s "]"))))

(defun matr-new (rows cols &optional (lst nil))
  "Примечание:
 (matr-new 3 4 '(1 2 3 4 5 6 7 8 9 10)) "
  (let ((mm (make-instance 'matrix :data (make-array (list rows cols) :initial-element 0.0d0)))
	(ll lst))
        (loop :for i :from 0 :below (array-dimension (matrix-data mm) 0)
       :do
	 (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1)
	    :do (setf (aref (matrix-data mm) i j) (car ll)
		      ll (cdr ll))))
	mm))

(defmethod initialize-instance ((mm matrix) &key (rows 3) (cols 3) initial-element initial-contents (element-type t))
  (cond
    (initial-element
      (setf (matrix-data mm)
	    (make-array (list rows cols)
			:element-type element-type :initial-element initial-element)))
    (initial-contents
      (setf (matrix-data mm)
	    (make-array (list rows cols)
			:element-type element-type :initial-contents initial-contents)))
    (t (setf (matrix-data mm)
	    (make-array (list rows cols)
			:element-type element-type :initial-element 0.0d0)))))

(defmethod matr-copy-* ((mm-ref matrix))
  (let* ((rows (matr-rows-* mm-ref))
	 (cols	(matr-cols-* mm-ref) )
	 (mm (make-instance 'matrix :data (make-array (list rows cols)))))
    (loop :for i :from 0 :below rows
       :do
	 (loop :for j :from 0 :below cols
	    :do (setf (aref (matrix-data mm) i j) (aref (matrix-data mm-ref) i j))))
    mm))

(defmethod matr-ij-*     ((mm matrix) i j) (aref (matrix-data mm) i j))

(defmethod matr-set-ij-* ((mm matrix) value row col) (setf (aref (matrix-data mm) row col) value) mm)

(defmethod matr-rows-*   ((mm matrix)) (array-dimension (matrix-data mm) 0))

(defmethod matr-cols-*   ((mm matrix)) (array-dimension (matrix-data mm) 1))

(defmethod matr-set-row-* ((mm matrix) row pts)
  (let ((data (matrix-data mm))
	(ll pts))
    (loop :for c :from 0 :below (matr-cols-* mm)
       :do (setf (aref data row c) (car ll)
		 ll (cdr ll)))))

(defmethod matr-get-row-* ((mm matrix) row)
  (let ((data (matrix-data mm)))
    (loop :for c :from 0 :below (matr-cols-* mm)
       :collect (aref data row c))))

(defmethod matr-set-col-* ((mm matrix) col pts)
    (let ((data (matrix-data mm))
	(ll pts))
    (loop :for r :from 0 :below (matr-rows-* mm)
       :do (setf (aref data r col) (car ll)
		 ll (cdr ll)))))

(defmethod matr-get-col-* ((mm matrix) col)
  (let ((data (matrix-data mm)))
    (loop :for r :from 0 :below (matr-rows-* mm)
       :collect (aref data r col))))

(defmethod major-diagonal ((mm matrix))
  "Извлекает главную диагональ матрицы
Пример использования:
 (defparameter *mm* (make-instance 'matrix :rows 4  :data '((1d0 2d0 3d0) (4d0 5d0 6d0) (7d0 8d0 9d0) (10d0 11d0 12d0)))
 =>
 Matr 4х3
 [ 1.0d0     2.0d0     3.0d0    ]
 [ 4.0d0     5.0d0     6.0d0    ]
 [ 7.0d0     8.0d0     9.0d0    ]
 [ 10.0d0    11.0d0    12.0d0   ]

 (major-diagonal *mm*)
 (1.0d0 5.0d0 9.0d0)
"
  (loop :for i :from 0 :below (min (matr-rows-* mm) (matr-cols-* mm))
     :collect (matr-ij-* mm i i)))



(defmethod minor-diagonal ((mm matrix))
  "Извлекает побочную диагональ матрицы
Пример использования:
 (defparameter *mm* (make-instance 'matrix :rows 4  :data '((1d0 2d0 3d0) (4d0 5d0 6d0) (7d0 8d0 9d0) (10d0 11d0 12d0))))
 =>
 Matr 4х3
 [ 1.0d0     2.0d0     3.0d0    ]
 [ 4.0d0     5.0d0     6.0d0    ]
 [ 7.0d0     8.0d0     9.0d0    ]
 [ 10.0d0    11.0d0    12.0d0   ]

 (minor-diagonal *mm*)

"  
  (loop
     :for c :from 0 :below (matr-cols-* mm)
     :for r :downfrom (- (matr-rows-* mm) 1) :to 0
     :collect (matr-ij-* mm c r)))

(defmethod matr-eval-* ((mm matrix))
  (let ((rows (matr-rows-* mm))
	(cols (matr-cols-* mm))
	(mm-cp (matr-copy-*  mm)))
  (loop :for i :from 0 :below rows
     :do
       (loop :for j :from 0 :below cols
	  :do (setf (aref (matrix-data mm) i j) (eval (aref (matrix-data mm) i j)))))
  mm-cp))

