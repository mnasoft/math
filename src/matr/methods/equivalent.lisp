;;;; ./src/matr/methods/equivalent.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; equivalent

(defmethod equivalent ((m1 <matrix>) (m2 <matrix>) &key (test #'math/core:semi-equal))
  (let ((rez t))
    (if (and (= (rows m1) (rows m2))
	     (= (cols m1) (cols m2)))
	(loop :for r :from 0 :below (rows m1) :do
	   (loop :for c :from 0 :below (cols m1) :do
		(setf rez (and rez (funcall test (mref m1 r c ) (mref m2 r c) )))))
	(setf rez nil))
    rez))

(defmethod equivalent ((a1 array) (a2 array) &key (test #'math/core:semi-equal))
   (declare (type (array * (* *)) a1 a2))
   (when (not (equal (array-dimensions a1)
		     (array-dimensions a2)))
     (return-from equivalent nil))
   (reduce #'(lambda (el1 el2) (and el1 el2))
	   (apply #'append
		  (loop :for i :from 0 :below (array-dimension a1 0)
			:collect
			(loop :for j :from 0 :below (array-dimension a1 1)
			      :collect
			      (funcall test (aref a1 i j) (aref a2 i j)))))
	   :initial-value t))

(defmethod equivalent ((m1 <matrix>) (a2 array) &key (test #'math/core:semi-equal))
  (declare (type (array * (* *)) a2))  
  (equivalent m1 (make-instance '<matrix> :data a2) :test test))

(defmethod equivalent ((a1 array) (m2 <matrix>) &key (test #'math/core:semi-equal))
  (declare (type (array * (* *)) a1))
  (equivalent (make-instance '<matrix> :data a1) m2 :test test))

;;;; (defmethod equivalent ((lst1 cons) (m2 <matrix>) &key (test #'math/core:semi-equal)))
;;;; (defmethod equivalent ((m1 <matrix>) (lst2 cons)  &key (test #'math/core:semi-equal)))
;;;; (defmethod equivalent ((lst1 cons) (a2 array) &key (test #'math/core:semi-equal)))
;;;; (defmethod equivalent ((a1 array) (lst2 cons)  &key (test #'math/core:semi-equal)))
