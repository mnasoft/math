;;;; ./src/tests/ls-gauss.lisp

(in-package #:math/tests)

(def-suite ls-gauss
  :description
  "Мастер-набор тестов по решению СЛАУ (системы линейных
 алгебраических уравнений."
  :in all)

(in-suite ls-gauss)

(def-test linear-system-convert-to-triangular-test ()
  "Пример использования: (test-convert-to-triangular)"
  (let ((m1 (make-instance
             'math/arr-matr:<matrix>
	     :initial-contents '((0.0 0.0 4.0 12.0)
				 (2.0 0.0 2.0  8.0)
				 (0.0 3.0 0.0  6.0))))
	(m1-tr (make-instance 'math/arr-matr:<matrix> :initial-contents
			      '(( 1.0 0.0 1.0 4.0 )
				( 0.0 1.0 0.0 2.0 )
				( 0.0 0.0 1.0 3.0 )))))
    (is-true
     (math/arr-matr:equivalent
      m1-tr
      (math/ls-gauss:convert-to-triangular m1)))))


(def-test backward-run-test ()
  "Пример использования: (test-matr-obrhod)"
  (let ((m1 (make-instance
             'math/arr-matr:<matrix>
	     :initial-contents '(( 1.0 0.0 1.0 4.0 )
				 ( 0.0 1.0 0.0 2.0 )
    				 ( 0.0 0.0 1.0 3.0 ))))
	(m1-obrhod (make-instance 'math/arr-matr:<matrix>
				  :initial-contents '(( 1.0 2.0 3.0 )))))
    (is-true
     (math/arr-matr:equivalent
      m1-obrhod
      (math/ls-gauss:backward-run m1)))))

(def-test solve-x-test ()
  "Пример использования: (test-solve-x)"
  (let ((m1 (make-instance
             'math/arr-matr:<matrix>
	     :initial-contents '((1 2 3 14)
				 (2 1 1  7)
				 (3 0 1  2))))
	(m1-gau
          (make-instance
           'math/arr-matr:<matrix>
	   :initial-contents '(( 1/3 16/3 1 )))))
    (is-true
     (math/arr-matr:equivalent
      (math/ls-gauss:solve-x m1) m1-gau))))

(def-test solve-x ()
  (is-true
   (math/core:semi-equal
    (let ((m '((1 2 3   14)
	       (2 1 1    7)  
	       (3 0 1    6))))
      (math/ls-gauss:solve-x m))
    #(1 2 3)))
  (loop :for i :from 1 :to 9 :do
    (let* ((m (math/rnd:make-2d-list i))
           (v (math/rnd:make-1d-list i))
           (m-v (math/rnd:make-ls-system m v)))
      (when (not (math/ls-gauss:singular-p  m-v))
        (is-true
         (math/core:semi-equal
          (math/ls-gauss:solve-x m-v)
          v))))))
