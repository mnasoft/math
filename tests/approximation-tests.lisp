;;;; tests/approximation-tests.lisp

(in-package #:math-tests)

(def-suite approximation-tests
  :description "Мастер-набор тестов по решению СЛАУ (систем линейных плгебраических уравнений."
  :in all-tests)

(in-suite  approximation-tests)

(def-test approximation-make-least-squares-matrix-test ()
  "Пример использования: (test-matr-mnk)"
  (let* ((m1 (make-instance 'math/arr-matr:<matrix>
			    :initial-contents '(( 98.0d0    34.0d0    14.0d0    98.0d0 )
						( 34.0d0    14.0d0    4.0d0     34.0d0 )
						( 14.0d0    4.0d0     4.0d0     14.0d0 ))))
	 (pts-1 '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0)))
	 (m2 (make-instance 'math/arr-matr:<matrix>
			    :initial-contents '(( 225.0d0   75.0d0    75.0d0    25.0d0    931.25d0 )
						( 75.0d0    75.0d0    25.0d0    25.0d0    606.25d0 )
						( 75.0d0    25.0d0    75.0d0    25.0d0    631.25d0 )
						( 25.0d0    25.0d0    25.0d0    25.0d0    406.25d0 ))))
	 (ff-2 #'(lambda (x1 x2)
		   (+ (* 1/2 x1 x1 )
		      (* 1/4 x2 x2 )
		      (* 1   x1 x2 )
		      (* 2   x1)
		      (* 3   x2)
		      8.0)))
	 (pts-2 (let ((rez nil))
		  (loop :for x1 :from -1 :to 3 :do
		    (loop :for x2 :from -1 :to 3 :do
		      (push (list x1 x2 (funcall ff-2 x1 x2)) rez )))
		  rez)))
    (is-true (math/arr-matr:equivalent
	      m1
	      (math/appr:make-least-squares-matrix '(xx yy) 
					      '((xx xx) (xx) (1.0) (yy)) 
					      pts-1 )))
    (is-true (math/arr-matr:equivalent
	      m2
	      (math/appr:make-least-squares-matrix '(x1 x2 yy) 
					      '((x1 x2)  (x1) (x2) (1.0) (yy)) 
					      pts-2)))))

(def-test averaging-function-tests ()
  "Пример использования: (test-averaging-function)"
  (is-true (equal
	  '((xx) (+ (* 1.0d0 xx xx) (* 0.0d0 xx) (* 0.0d0 1.0)))
	  (math/appr::averaging-function-body '(xx yy) 
			  '((xx xx) (xx) (1.0) (yy)) 
			  '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0)))))
  (is-true (equal
	  '(lambda (xx) (+ (* 1.0d0 xx xx) (* 0.0d0 xx) (* 0.0d0 1.0)))
	  (math/appr:averaging-function-lambda '(xx yy) 
			    '((xx xx) (xx) (1.0) (yy)) 
			    '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0)))))
  (is-true (equal
	  '(defun coool-func (xx) (+ (* 1.0d0 xx xx) (* 0.0d0 xx) (* 0.0d0 1.0)))
	  (math/appr:averaging-function-defun '(xx yy) 
			  '((xx xx) (xx) (1.0) (yy)) 
			  '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0))
			  'coool-func))))
