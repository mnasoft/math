;;;; ./src/matr/methods/rotate.lisp
(in-package :math/matr)

(defmethod rotate-x ((α number))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (progn (defparameter *p* (make-instance 'math/matr:<matrix> :dimensions '(1 4)))
        (setf (math/matr:row *p* 0) '(10.0 20.0 30.0 1.0))
        (multiply *p* (rotate-y (dtr 90.0))))
@end(code)"
  (let ((matrix (make-instance '<matrix> :dimensions '(4 4))))
    (setf (row matrix 0) `(1.0     0.0         0.0   0.0))
    (setf (row matrix 1) `(0.0 ,(cos α) ,(- (sin α)) 0.0))
    (setf (row matrix 2) `(0.0 ,(sin α)    ,(cos α)  0.0))
    (setf (row matrix 3) `(0.0     0.0         0.0   1.0))
    matrix))

(defmethod rotate-y ((β number))
  " @b(Пример использования:)
@begin[lang=lisp](code)

@end(code)
"
  (let ((matrix (make-instance '<matrix> :dimensions '(4 4))))
    (setf (row matrix 0) `(   ,(cos β)  0.0 ,(sin β) 0.0))
    (setf (row matrix 1) `(       0.0   1.0     0.0  0.0))
    (setf (row matrix 2) `(,(- (sin β)) 0.0 ,(cos β) 0.0))
    (setf (row matrix 3) `(       0.0   0.0     0.0  1.0))
    matrix))

(defmethod rotate-z ((γ number))
  " @b(Пример использования:)
@begin[lang=lisp](code)

@end(code)"
  (let ((matrix (make-instance '<matrix> :dimensions '(4 4))))
    (setf (row matrix 0) `(,(cos γ) ,(- (sin γ)) 0.0 0.0))
    (setf (row matrix 1) `(,(sin γ)    ,(cos γ)  0.0 0.0))
    (setf (row matrix 2) `(    0.0         0.0   1.0 0.0))
    (setf (row matrix 3) `(    0.0         0.0   0.0 1.0))
    matrix))

(defmethod rotate-v ((θ number) (v cons))
  " @b(Пример использования:)
@begin[lang=lisp](code)

@end(code)"
  (let ((matrix (make-instance '<matrix> :dimensions '(4 4)))
        (x (first  v))
        (y (second v))
        (z (third  v)))
    (setf (row matrix 0) `(,(+       (cos θ) (* (- 1 (cos θ)) x x)) ,(- (* (- 1 (cos θ)) x y) (* (sin θ) z)) ,(+ (* (- 1 (cos θ)) x z) (* (sin θ) y)) 0.0))
    (setf (row matrix 1) `(,(+ (* (- 1 (cos θ)) y x) (* (sin θ) z))       ,(+ (cos θ) (* (- 1 (cos θ)) y y)) ,(- (* (- 1 (cos θ)) y z) (* (sin θ) x)) 0.0))
    (setf (row matrix 2) `(,(- (* (- 1 (cos θ)) z x) (* (sin θ) y)) ,(+ (* (- 1 (cos θ)) z y) (* (sin θ) x))       ,(+ (cos θ) (* (- 1 (cos θ)) z z)) 0.0))
    (setf (row matrix 3) `(                                     0.0                                      0.0                                      0.0 1.0))
    matrix))

(defmethod rotate-around ((point-1 cons) (point-2 cons) θ)
  " @b(Пример использования:)
@begin[lang=lisp](code)

@end(code)"
  (let ((rotate-p1-p2 (rotate-v θ (normalize (mapcar #'- point-2 point-1))))
        (move-p1-p0   (move-v (mapcar #'- point-1)))
        (move-p0-p1   (move-v point-1)))
    (multiply
     (multiply move-p1-p0 rotate-p1-p2)
     move-p0-p1)))
