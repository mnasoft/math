;;;; ./src/matr/methods/dimensions.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; dimensions

(defmethod dimensions ((2d-list cons))
  " @b(Пример использования:)
@begin[lang=lisp](code)
  (dimensions '((1 2 3)
                (4 5 6))) 
@end(code)"
  (list (rows 2d-list) (cols 2d-list)))

(defmethod dimensions ((a array))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (dimensions
  (make-array '(2 3)
              :initial-contents '((1 2 3)
                                  (4 5 6)))) => (2 3)
@end(code)"
  (when (/= (array-rank a) 2) (error 'dimensions-operation-not-appicable))
  (array-dimensions a))

(defmethod dimensions ((mm <matrix>))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (dimensions
  (make-instance '<matrix>
                 :initial-contents '((1 2 3)
                                     (4 5 6 ))))
@end(code)"
  (array-dimensions (matrix-data mm)))
