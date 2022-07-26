;;;; ./math/src/tests/equation.lisp

(in-package #:math/tests)

(def-suite equation
  :description "Мастер-набор всех тестов math/equation."
  :in all)

(in-suite equation)

#+nil
(
 (math/equation:roots (make-instance 'math/equation:<linear>  :a 1d0 :b -1d0))
 (math/equation:roots (make-instance 'math/equation:<quadric> :a 1d0 :b -3d0 :c 2d0))
 (math/equation:roots (make-instance 'math/equation:<cubic>   :a 1d0 :b -6d0 :c 11d0 :d -6d0))
 (math/equation:roots (make-instance 'math/equation:<quartic> :a 1 :b -10 :c 35 :d -50 :e 24))
 )

(def-test equation-linear-coeffs ()
  "def-test equation-linear-coeffs ()"
  (let ((linear (make-instance 'math/equation:<linear>  :a 1d0 :b -1d0)))
    (is-true (equalp (math/equation:coeff-a linear) 1d0))
    (is-true (equalp (math/equation:coeff-b linear) -1d0))
;;;; tab    
    (is-true  (math/core:semi-equal (math/equation:tab linear 1d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab linear 0.5d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab linear 1.5d0) 0d0))
;;;; roots
    (is-true (math/core:semi-equal (math/equation:roots linear)  1d0))
    ))

(def-test equation-quadric-coeffs ()
  "def-test equation-quadric-coeffs ()"
  (let ((quadric (make-instance 'math/equation:<quadric> :a 1d0 :b -3d0 :c 2d0)))
    (is-true (equalp (math/equation:coeff-a quadric)  1d0))
    (is-true (equalp (math/equation:coeff-b quadric) -3d0))
    (is-true (equalp (math/equation:coeff-c quadric)  2d0))
;;tab
    (is-true (math/core:semi-equal  (math/equation:tab quadric 1d0) 0d0))
    (is-true (math/core:semi-equal  (math/equation:tab quadric 2d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab quadric 0.5d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab quadric 1.5d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab quadric 2.5d0) 0d0))
;;;; roots
    (is-true (math/core:semi-equal (sort (math/equation:roots quadric) #'<) '(1 2)))))

(def-test equation-cubic-coeffs ()
  "def-test equation-cubic-coeffs ()"
  (let ((cubic (make-instance 'math/equation:<cubic> :a 1d0 :b -6d0 :c 11d0 :d -6d0)))
    (is-true (equalp (math/equation:coeff-a cubic)  1d0))
    (is-true (equalp (math/equation:coeff-b cubic) -6d0))
    (is-true (equalp (math/equation:coeff-c cubic) 11d0))
    (is-true (equalp (math/equation:coeff-d cubic) -6d0))
;;;; tab
    (is-true (math/core:semi-equal  (math/equation:tab cubic 1d0) 0d0))
    (is-true (math/core:semi-equal  (math/equation:tab cubic 2d0) 0d0))
    (is-true (math/core:semi-equal  (math/equation:tab cubic 3d0) 0d0))

    (is-false (math/core:semi-equal (math/equation:tab cubic 0.5d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab cubic 1.5d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab cubic 2.5d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab cubic 3.5d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab cubic 4.5d0) 0d0))
;;;; roots
    (is-true (math/core:semi-equal
              (sort (math/equation:roots cubic) #'< :key #'math/core:norma)
              '(1 2 3)))))

(def-test equation-quartic-coeffs ()
  "def-test equation-quartic-coeffs ()"
  (let ((quartic (make-instance 'math/equation:<quartic> :a 1 :b -10 :c 35 :d -50 :e 24)))
    (is-true (equalp (math/equation:coeff-a quartic)   1d0))
    (is-true (equalp (math/equation:coeff-b quartic) -10d0))
    (is-true (equalp (math/equation:coeff-c quartic)  35d0))
    (is-true (equalp (math/equation:coeff-d quartic) -50d0))
    (is-true (equalp (math/equation:coeff-e quartic)  24d0))
;;;; tab
    (is-true (math/core:semi-equal (math/equation:tab quartic 1d0) 0d0))
    (is-true (math/core:semi-equal (math/equation:tab quartic 2d0) 0d0))
    (is-true (math/core:semi-equal (math/equation:tab quartic 3d0) 0d0))
    (is-true (math/core:semi-equal (math/equation:tab quartic 4d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab quartic 1.5d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab quartic 2.5d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab quartic 3.5d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab quartic 4.5d0) 0d0))
    (is-false (math/core:semi-equal (math/equation:tab quartic 5.5d0) 0d0))
;;;; roots
    #+nil
    (is-true (math/core:semi-equal
              (sort (math/equation:roots quartic) #'< :key #'math/core:norma)
              '(1 2 3 4)))))




