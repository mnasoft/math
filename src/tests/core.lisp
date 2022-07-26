;;;; ./math/src/tests/core.lisp

(in-package #:math/tests)

(def-suite core
  :description "Мастер-набор всех тестов math/core."
  :in all)

(in-suite core)

#+nil
((math/core:roots (make-instance 'math/core:<linear>  :a 1d0 :b -1d0))
 (math/core:roots (make-instance 'math/core:<quadric> :a 1d0 :b -3d0 :c 2d0))
 (math/core:roots (make-instance 'math/core:<cubic>   :a 1d0 :b -6d0 :c 11d0 :d -6d0))
 (math/core:roots (make-instance 'math/core:<quartic> :a 1 :b -10 :c 35 :d -50 :e 24)))

(def-test norma ()
  (is-true (= 0 (math/core:norma 0)))
  (is-true (= 100 (math/core:norma 100)))
  (is-true (= 100e0 (math/core:norma 100e0)))
  (is-true (= 100d0 (math/core:norma 100d0)))
  (is-true (= 2.236068
              (math/core:norma #C(1 2))
              (math/core:norma #C(2 1))
              (math/core:norma #C(-1 2))
              (math/core:norma #C(-2 1))))
  (is-true (= 3/2 (math/core:norma '(-2 1))
              (math/core:norma '(-1 2))))
  (is-true (= 2
              (math/core:norma '(1 2 3))
              (math/core:norma '(-1 2 -3))))
    (is-true (= 2.868034
              (math/core:norma '(#C(1 2) #C(2 1) 2 5))
              (math/core:norma '(#C(-1 2) #C(-2 1) -2 5)))))

(def-test distance ()
  "distance ()"
  (is-true (= 0 (math/core:distance  0 0)))
  (is-true (= 1 (math/core:distance  1 0)))
  (is-true (= 1 (math/core:distance -1 0)))
  (is-true (= 0e0 (math/core:distance  0e0 0e0)))
  (is-true (= 1e0 (math/core:distance  1e0 0e0)))
  (is-true (= 1e0 (math/core:distance -1e0 0e0)))
  (is-true (= 1e0 (math/core:distance -1e0 0e0)))
  (is-true (= 0.0 (math/core:distance '(1 2 3) '(1 2 3))))
  (is-true (= 3.7416575 (math/core:distance '(2 4 6) '(1 2 3))))
  (is-true (= 3.8729835 (math/core:distance '(#C(2 1) 4 6) '(#C(1 2) 2 3)))))

(def-test distance-relative ()
  "distance ()"
  (is-true (= 0 (math/core:distance-relative  0 0 10)))
  (is-true (= 0.1 (math/core:distance-relative  1 0 10)))
  (is-true (= 0.1 (math/core:distance-relative -1 0 10)))
  (is-true (= 0.0 (math/core:distance-relative  0e0 0e0 10)))
  (is-true (= 0.2 (math/core:distance-relative  1e0 0e0 5)))
  (is-true (= 0.1 (math/core:distance-relative -1e0 0e0 10)))
  (is-true (= 0.2 (math/core:distance-relative -1e0 0e0 5))))

(def-test semi-equal ()
  "def-test semi-equal ()"
  (is-true  (math/core:semi-equal 1.0 2.0 :tolerance 1.001))
  (is-true  (math/core:semi-equal 1.0 2.0 :tolerance 1.000001))
  (is-false (math/core:semi-equal 1.0 1.1))
  (is-false (math/core:semi-equal 1.0 1.01))
  (is-false (math/core:semi-equal 1.0 1.001))
  (is-false (math/core:semi-equal 1.0 1.0001))
  (is-false (math/core:semi-equal 1.0 1.00001))
  (is-true  (math/core:semi-equal 1.0 1.000001))
  (is-true  (math/core:semi-equal 1.0 1.0000001))
  )
