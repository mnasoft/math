;;;; tests/main.lisp

(in-package #:math-tests)

(def-suite all-tests
  :description "Мастер-набор всех тестов проекта math.")

(in-suite all-tests)

(defun test-math ()
  (run! 'all-tests))
