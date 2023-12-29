;;;; ./src/matr/methods/swap-cols.lisp
(in-package :math/matr)

(defmethod swap-cols* ((mm <matrix>) i j)
  (assert (and (< -1 i (cols mm)) (< -1 j (cols mm))))
  (when (/= i j)
    (let ((col-i (col mm i))
	  (col-j (col mm j)))
      (setf (col mm i) col-j
	    (col mm j) col-i)))
  mm)

(defmethod swap-cols ((mm <matrix>) i j)
  (swap-cols* (copy mm) i j))
