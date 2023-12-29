;;;; ./src/matr/methods/average.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; average - Вычисление среднего значения
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod average-value ((2d-list cons))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (average-value '((1.0 1.5 2.0)
                  (2.0 2.5 3.0))) => 2.0
@end(code)"  
  (math/stat:average-value (unite-rows 2d-list)))

(defmethod average-not-nil-value ((2d-list cons))
  (math/stat:average-not-nil-value (unite-rows 2d-list)))

;;;;

(defmethod average-row-value ((2d-list cons))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (average-row-value '((1.0 1.5 2.0)
                      (2.0 2.5 3.0))) => (1.5 2.5)
@end(code)"  
  (mapcar #'math/stat:average-value 2d-list))

(defmethod average-row-not-nil-value ((2d-list cons))
  (mapcar #'math/stat:average-not-nil-value 2d-list))

;;;;

(defmethod average-col-value ((2d-list cons))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (average-col-value '((3.0 2.1 4.5)
                     (2.0 2.5 3.2))) => (2.5 2.3 3.85)
@end(code)"  
  (average-row-value (transpose 2d-list)))

(defmethod average-col-not-nil-value ((2d-list cons))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (average-col-not-nil-value '((nil 2.1 4.5)
                             (2.0 nil 3.2))) => (2.0 2.1 3.85)
  
@end(code)"  
  (average-row-not-nil-value (transpose 2d-list)))
