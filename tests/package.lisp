;;;; tests/package.lisp

(defpackage #:math-tests
  (:use #:cl #:fiveam)
  (:export #:run!
	   #:all-tests
	   #:test-math))
