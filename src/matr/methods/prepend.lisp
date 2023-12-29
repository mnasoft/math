;;;; ./src/matr/methods/prepend.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; prepend

(defmethod prepend-col ((col cons) (matrix cons))
 " @b(Пример использования:)
@begin[lang=lisp](code)
 (prepend-col '(10 11 12)
              '((1  2  3)
                (4  5  6)
                (7  8  9))) => ((10 1 2 3)
                                (11 4 5 6)
                                (12 7 8 9))
@end(code)"  
  (let ((rez nil)
        (r nil))
    (dolist (l matrix (reverse rez))
      (setf r (car col)
            col (cdr col))
      (push (cons r l) rez))))

(defmethod prepend-row ((row cons) (matrix cons))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (prepend-row '(10 11 12) 
              '((1 2 3)
                (4 5 6)
                (7 8 9))) =>((10 11 12) 
                             ( 1  2  3) 
                             ( 4  5  6)
                             ( 7  8  9))
 (prepend-row '(10 11)
              '((1 2 3)
                (4 5 6) 
                (7 8 9))) =>((10 11 NIL) 
                             ( 1  2   3) 
                             ( 4  5   6) 
                             ( 7  8   9))
 (prepend-row '(10 11 12 13)
              '((1  2  3)
                (4  5  6)
                (7  8  9))) =>((10 11 12)
                               ( 1  2  3) 
                               ( 4  5  6) 
                               ( 7  8  9))
@end(code)"
  (transpose (prepend-col row (transpose matrix))))

(defmethod prepend-rows ((rows cons) (matrix cons))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (prepend-rows
 '((10 20 30)
   (11 22 33))
 '((11 12 13)
   (12 13 14)
   (13 14 15)))
@end(code)"  
  (reduce
   #'(lambda (x y)
       (prepend-row y x))
   (reverse rows) :initial-value matrix))

