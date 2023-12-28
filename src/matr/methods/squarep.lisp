;;;; ./src/matr/methods/squarep.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; squarep

(defmethod squarep ((mm <matrix>))
  " @b(Пример использования:)
@begin[lang=lisp](code)
  (defparameter *mm* (make-instance '<matrix>   :dimensions '(2 3)))
  (squarep *mm*) => NIL
  (defparameter *mm* (make-instance '<matrix>   :dimensions '(3 3)))
  (squarep *mm*) => T
@end(code)"
  (= (cols mm) (rows mm) ))

(defmethod squarep ((a array))
  " @b(Пример использования:)
@begin[lang=lisp](code)
  (defparameter *aa*
    (make-array '(2 3) :initial-contents '((1 2 3)(4 5 6))))
  (squarep *aa*) => NIL

  (defparameter *aa*
    (make-array '(3 3) :initial-contents '((1 2 3)(4 5 6) (7 8 9))))
  (squarep *aa*) => T
@end(code)"
  (= (cols a) (rows a)))

(defmethod squarep ((lst cons))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (squarep '((1 2 3)(4 5 6))) => NIL
 (squarep '((1 2 3)(4 5 6) (7 8 9))) => T
@end(code)"
  (= (cols lst) (rows lst)))
