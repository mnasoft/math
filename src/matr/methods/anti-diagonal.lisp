;;;; ./src/matr/methods/anti-diagonal.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; anti-diagonal

(defmethod anti-diagonal ((mm <matrix>))
  "@b(Пример использования:)
@begin[lang=lisp](code)
 (defparameter *mm* 
  (make-instance '<matrix> 
   :initial-contents '((1d0 2d0 3d0) 
                       (4d0 5d0 6d0) 
                       (7d0 8d0 9d0))))
 =>
 Matr 3х3
 [ 1.0d0     2.0d0     3.0d0    ]
 [ 4.0d0     5.0d0     6.0d0    ]
 [ 7.0d0     8.0d0     9.0d0    ]

  (anti-diagonal  *mm*) => (3.0d0 5.0d0 7.0d0)
  @end(code)"
  (assert (squarep mm) (mm) "Матрица не является квадратной~%~S" mm)
  (loop
    :for c :from 0 :below (cols mm)
    :for r :downfrom (- (rows mm) 1) :to 0
    :collect (mref mm c r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (setf anti-diagonal)

(defmethod (setf anti-diagonal) (elements (mm <matrix>) )
  (assert (squarep mm) (mm) "Матрица не является квадратной~%~S" mm)
  (loop
     :for c :from 0 :below (cols mm)
     :for r :downfrom (- (rows mm) 1) :to 0
     :for e :in elements :by #'cdr :do
       (setf (mref mm c r) e))
  mm)
