;;;; tests/array.lisp

(in-package #:math/tests)

(def-suite half-div
  :description "Мастер-набор всех тестов проекта math/matr для матрицы
  типа 2d-array."
  :in all)

(in-suite half-div)

(def-test h-div-h-div-lst ()
      (is-true (math/core:semi-equal
              1382.7728
              (math/half-div:h-div-lst
                         2.0d0 100000.0d0
                         #'(lambda (x y) (- (* x (log x)) y))
                         0
                         '(t 10000.)
                         :eps 1d-10
                         :iters 50))))

(def-test h-div-h-div ()
  (is-true (math/core:semi-equal
            1382.7728
            (math/half-div:h-div 2.0 100000.0
                                 #'(lambda (x)
                                     (- (* x (log x)) 10000.))
                                 :iters 50))))






