;;;; ./src/matr/methods/cols.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cols

(defmethod cols ((a array))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (To-Do)
@end(code)
"
  (assert (= (array-rank a) 2))
  (array-dimension a 1))

(defmethod cols ((2d-list cons))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (To-Do)
@end(code)
"
  (math/stat:min-value (mapcar #'length 2d-list)))

(defmethod cols ((mm <matrix>))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (To-Do)
@end(code)
"
  (array-dimension (matrix-data mm) 1))
