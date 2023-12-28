;;;; ./src/matr/methods/add.lisp
(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; add

(defmethod add ((a <matrix> ) (b <matrix>))
  " @b(Пример использования:)

@begin[lang=lisp](code)
 (add (matr-new 2 2 '(1 2 
                      3 4)) 
      (matr-new 2 2 '(1 2 
                      3 4)))
  => Matr 2х2
     [ 2         4        ]
     [ 6         8        ]
 (add (matr-new 2 2 '(1 2 3 4)) (matr-new 2 2 '(4 3 2 1)))
  => Matr 2х2
     [ 5         5        ]
     [ 5         5        ]
@end(code)
"
  (assert (and (= (rows a) (rows b)) (= (cols a) (cols b))) (a b)
	  "Матрицы A[~A,~A] и B[~A,~A] имеют отличаюшиеся размеры"
	  (rows a) (cols a) (rows b) (cols b))
  (let ((a+b (matr-new (rows a) (cols a))))
    (loop :for r :from 0 :below (rows a) :do
      (loop :for c :from 0 :below (rows b) :do
	(setf (mref a+b r c)
	      (+ (mref a r c ) (mref b r c)))))
    a+b))
