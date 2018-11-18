;;;; x-o-testing.lisp

(in-package #:math)

(play)

(game-over-p *xo*)

(progn (matr-set-row-* *xo* 0 '(0 2 0))
       (matr-set-row-* *xo* 1 '(0 0 0)) 
       (matr-set-row-* *xo* 2 '(0 0 0)))


