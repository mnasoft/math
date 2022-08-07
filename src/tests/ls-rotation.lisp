;;;; ./src/tests/ls-rotation.lisp

(in-package #:math/tests)

(def-suite ls-rotation
  :description "Мастер-набор всех тестов math/ls-rotation решение системы
  линейных уравнений методом вращения."
  :in all)

(in-suite ls-rotation)

(def-test ls-rotation-solve-x ()
  (is-true
   (math/core:semi-equal
    (let ((m '((1 2 3   14)
	       (2 1 1    7)  
	       (3 0 1    6))))
      (math/ls-rotation:solve-x m))
    #(1 2 3)))
  (loop :for i :from 1 :to 9 :do
    (let* ((m (math/rnd:make-2d-list i))
           (v (math/rnd:make-1d-list i))
           (m-v (math/rnd:make-ls-system m v)))
      (when (not (math/ls-gauss:singular-p  m-v))
        (is-true
         (math/core:semi-equal
          (math/ls-rotation:solve-x m-v)
          v))))))

