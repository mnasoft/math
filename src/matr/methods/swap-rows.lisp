;;;; ./src/matr/methods/swap-rows.lisp
(in-package :math/matr)

(defmethod swap-rows*  ((mm <matrix>) i j)
  (assert (and (< -1 i (rows mm)) (< -1 j (rows mm))))
  (when (/= i j)
    (let ((row-i (row mm i))
	  (row-j (row mm j)))
      (setf (row mm i) row-j
	    (row mm j) row-i)))
  mm)

(defmethod swap-rows  ((mm <matrix>) i j)
  (swap-rows* (copy mm) i j))
