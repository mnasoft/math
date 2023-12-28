;;;; ./src/matr/methods/row.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; row

(defmethod row ((mm <matrix>) row)
  (let ((data (matrix-data mm)))
    (loop :for c :from 0 :below (cols mm)
	  :collect (aref data row c))))

(defmethod row ((row integer) (a array))
  " @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((arr (make-array '(5 2)
			 :initial-contents '((0 1)
					     (2 3)
					     (4 5)
					     (6 7)
					     (8 9)))))
   (row  0 arr)		    ;=> #(0 1)
   (row  2 arr)	            ;=> #(4 5)
   (row  4 arr)		    ;=> #(8 9))
@end(code)"
  (when (/= (array-rank a) 2) (error 'row-operation-not-appicable))
  (assert (< -1 row (array-dimension a 0)))
  (apply #'vector (loop :for j :from 0 :below (array-dimension a 1)
			:collect
			(aref a row j))))


(defmethod row (row (2d-list cons))
  (nth row 2d-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (setf row)

(defmethod (setf row) (new-value-lst (mm <matrix>) row )
  (let ((data (matrix-data mm))
	(ll new-value-lst))
    (loop :for c :from 0 :below (cols mm)
       :do (setf (aref data row c) (car ll)
		 ll (cdr ll)))
    mm))

(defmethod (setf row) ((new-row cons) (a array) row)
  (assert (= (array-rank a) 2))
  (assert (< -1 row (rows a)))
  (assert (= (cols a) (length new-row)))
  (let ((ll new-row))
    (loop :for c :from 0 :below (cols a)
       :do (setf (aref a row c) (car ll)
		 ll (cdr ll)))
    a))

(defmethod (setf row) ((new-val number) (a array) row)
  (assert (= (array-rank a) 2))
  (assert (< -1 row (rows a)))
  (loop :for c :from 0 :below (cols a) :do (setf (aref a row c) new-val))
  a)

(defmethod (setf row) ((new-row vector) (a array) row)
  (assert (= (array-rank a) 2))
  (assert (< -1 row (rows a)))
  (assert (= (cols a) (length new-row)))
  (loop :for c :from 0 :below (cols a) :do (setf (aref a row c) (svref new-row c)))
  a)
