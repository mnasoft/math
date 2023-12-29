;;;; ./src/matr/methods/append.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; append

(defmethod append-col ((c-lst cons) (2d-list cons))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (math:append-col '(10 11 12)
                  '((1 2 3)
                    (4 5 6)
                    (7 8 9))) => ((1 2 3 10) 
                                  (4 5 6 11)
                                  (7 8 9 12))
@end(code)"  
  (let ((rez nil)
        (r nil))
    (dolist (l 2d-list (reverse rez))
      (setf r (car c-lst)
            c-lst (cdr c-lst))
      (push (append l (list r)) rez))))

(defmethod append-row ((c-lst cons) (2d-list cons))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (math:append-row '(10 11 12)
                  '((1 2 3)
                    (4 5 6)
                    (7 8 9)))  =>((1 2 3)
                                  (4 5 6)
                                  (7 8 9)
                                  (10 11 12))

 (math:append-row '(10 11 )
                  '((1 2 3)
                    (4 5 6)
                    (7 8 9)))
 =>((1 2 3)
    (4 5 6)
    (7 8 9)
    (10 11 NIL))

 (math:append-row '(10 11 12 13)
                  '((1 2 3)
                    (4 5 6)
                    (7 8 9))) =>(( 1  2  3)
                                 ( 4  5  6)
                                 ( 7  8  9)
                                 (10 11 12))
@end(code)"  
  (transpose (append-col c-lst (transpose 2d-list))))
