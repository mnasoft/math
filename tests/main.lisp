;;;; tests/main.lisp

(in-package #:math-tests)

(def-suite all-tests
  :description "Мастер-набор всех тестов проекта math.")

(in-suite all-tests)

(defun test-math ()
  (run! 'all-tests))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(require :sb-cover)
(require :math/tests)
(declaim (optimize sb-cover:store-coverage-data))
(asdf:oos 'asdf:load-op :math/tests :force t)
(sb-cover:report "coverage/")



|#
