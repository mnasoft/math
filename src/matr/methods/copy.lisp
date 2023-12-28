;;;; ./src/matr/methods/copy.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; copy

(defmethod copy ((mm-ref <matrix>))
  (make-instance '<matrix>
                 :data (cl-utilities:copy-array (matrix-data mm-ref))))

(defmethod copy ((mm-ref array))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (let* (
      (a1 (make-array '(2 3) :initial-contents '((1 2 3)(4 5 6))))
      (a2 (copy a1)))
  (setf (mref a2 1 1) 11.55)
  (values a2 a1)
  )
@end(code)"
  (cl-utilities:copy-array mm-ref))
