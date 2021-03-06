;;;; tests/linear-system-tests.lisp

(in-package #:math-tests)

(def-suite linear-system-tests
  :description "Мастер-набор тестов по решению СЛАУ (системы линейных алгебраических уравнений."
  :in all-tests)

(in-suite linear-system-tests)

(def-test linear-system-convert-to-triangular-test ()
  "Пример использования: (test-convert-to-triangular)"
  (let ((m1 (make-instance 'math/arr-matr:<matrix>
			   :initial-contents '((0.0 0.0 4.0 12.0)
					       (2.0 0.0 2.0  8.0)
					       (0.0 3.0 0.0  6.0))))
	(m1-tr (make-instance 'math/arr-matr:<matrix> :initial-contents
			      '(( 1.0 0.0 1.0 4.0 )
				( 0.0 1.0 0.0 2.0 )
				( 0.0 0.0 1.0 3.0 )))))
    (is-true (math/arr-matr:equivalent m1-tr (math/arr-matr:convert-to-triangular m1)))))


(def-test solve-linear-system-gauss-backward-run-test ()
  "Пример использования: (test-matr-obrhod)"
  (let ((m1 (make-instance 'math/arr-matr:<matrix>
			   :initial-contents '(( 1.0 0.0 1.0 4.0 )
					       ( 0.0 1.0 0.0 2.0 )
    					       ( 0.0 0.0 1.0 3.0 ))))
	(m1-obrhod (make-instance 'math/arr-matr:<matrix>
				  :initial-contents '(( 1.0 2.0 3.0 )))))
    (is-true (math/arr-matr:equivalent m1-obrhod
			      (math/ls-gauss::solve-linear-system-gauss-backward-run m1)))))

(def-test solve-linear-system-gauss-test ()
  "Пример использования: (test-solve-linear-system-gauss)"
  (let ((m1 (make-instance 'math/arr-matr:<matrix>
			   :initial-contents '((1 2 3 14)
						       (2 1 1  7)
						       (3 0 1  2))))
	(m1-gau (make-instance 'math/arr-matr:<matrix>
			       :initial-contents '(( 1/3 16/3 1 )))))
    (is-true (math/arr-matr:equivalent (math/ls-gauss:solve-linear-system-gauss m1) m1-gau))))

