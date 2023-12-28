;;;; ./src/matr/methods/mref.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; mref

(defmethod mref ((mm <matrix>) i j)
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (mref (matr-new 2 3 '(1 2 3 4 5 6)) 1 2) => 6 
@end(code)"
  (aref (matrix-data mm) i j))

(defmethod mref ((mm cons) i j)
  (nth j (nth i mm)))

(defmethod mref ((mm array) i j)
  (aref mm i j))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (setf mref)

(defmethod (setf mref) (value (mm <matrix>) i j)
  (setf (aref (matrix-data mm) i j) value)
  mm)

(defmethod (setf mref) (value (mm cons) i j)
  (setf (nth j (nth i mm)) value)
  mm)

(defmethod (setf mref) (value (mm array) i j)
  (setf (aref mm i j) value)
  mm)
