;;;; ./math/src/tests/core.lisp

(in-package #:math/tests)

(def-suite ls-gsll
  :description "Мастер-набор всех тестов math/core."
  :in all)

(in-suite ls-gsll)


(def-test ls-solve ()
  (is-true
   (math/core:semi-equal
    (let ((m '((1 2 3   14)
	       (2 1 1    7)  
	       (3 0 1    6))))
      (math/ls-gsll:solve-x m))
    #(1 2 3)))
  (loop :for i :from 1 :to 9 :do
    (let* ((m (math/rnd:make-2d-list i))
           (v (math/rnd:make-1d-list i))
           (m-v (math/rnd:make-ls-system m v)))
      (when (not (math/ls-gauss:singular-p  m-v))
        (is-true
         (math/core:semi-equal
          (math/ls-gsll:solve-x m-v)
          v))))))

