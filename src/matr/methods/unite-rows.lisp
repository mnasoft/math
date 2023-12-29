;;;; ./src/matr/methods/unite-rows.lisp
(in-package :math/matr)

(defmethod unite-rows ((2d-list cons))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (unite-rows '((1 2 3)
               (4 5 6)))
 =>(1 2 3 4 5 6)
@end(code)"  
  (let ((rez nil))
    (mapcar #'(lambda (el) (setf rez (append rez el) ) ) 2d-list)
    rez))
