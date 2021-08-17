;;;; tests/package.lisp

(defpackage #:math/tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package :math/tests)

(defun run-tests () (run! 'all))

;;;;(run-tests)

