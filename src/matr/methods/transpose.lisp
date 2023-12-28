;;;; ./src/matr/methods/transpose.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; transpose

(defmethod transpose ((mm <matrix>))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (transpose (matr-new 2 3 '(1 2 3
                            4 5 6)))
  => Matr 3х2
     [ 1         4        ]
     [ 2         5        ]
     [ 3         6        ]
@end(code)"
  (let ((rez (make-instance '<matrix> :dimensions (nreverse (dimensions mm)))))
    (loop :for i :from 0 :below (rows mm) :do
      (loop :for j :from 0 :below (cols mm) :do
	(setf (mref rez j i) (mref mm i j))))
    rez))

(defmethod transpose ((mm cons))
  "Выполняет транспонирование"
  (apply #'mapcar #'list mm))
