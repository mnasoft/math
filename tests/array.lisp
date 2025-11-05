;;;; tests/array.lisp

(in-package :math/tests)

(def-suite 2d-array
  :description "Мастер-набор всех тестов проекта math/matr для матрицы
  типа 2d-array."
  :in all)

(in-suite 2d-array)

(def-test array-get-row ()
  "Проверка доступа к строкам."
  (let ((arr (make-array '(5 2)
			 :initial-contents '((0 1)
					     (2 3)
					     (4 5)
					     (6 7)
					     (8 9))))
	(arr-1 (make-array '(5 2 3) :initial-element 0)))
;;;(signal (row -1 arr)) ; => error
;;;(signal (row  5 arr)) ; => error
    (is-true (equalp (math:row  0 arr) #(0 1)))
    (is-true (equalp (math:row  2 arr) #(4 5)))
    (is-true (equalp (math:row  4 arr) #(8 9)))
;;; (is-true (equal (row 2 arr-1)))
    ))
