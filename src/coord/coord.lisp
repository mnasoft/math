;;;; ./src/coord/coordinate-system.lisp
 
(defpackage #:math/coord
  (:use #:cl #:math/core)
  (:export dtr
           rtd
           polar->cartesian
           cartesian->polar
           spherical->cartesian
           cartesian->spherical
	   )
  (:documentation "coord"))

(in-package :math/coord)

(defun dtr (degree)
  (* pi 1/180 degree ))

(defun rtd (radian)
  (/ radian pi 1/180))

(defun polar->cartesian (radius-angle)
  (let ((radius (first  radius-angle))
	(angle  (second radius-angle)))
    (list (* radius (cos angle)) (* radius (sin angle)))))

(defun cartesian->polar (x-y)
  (let ((radius (sqrt (apply #'+ (mapcar #'square x-y))))
	(angle  (atan (second x-y) (first x-y))))
    (list radius angle)))

(defun spherical->cartesian (r φ θ)
  (let ((x (* r (cos φ) (sin θ)))
	(y (* r (sin φ) (sin θ)))
	(z (* r (cos θ))))
    (list x y z)))

(defun cartesian->spherical (x-y-z)
  (let* ((x (first  x-y-z))
	 (y (second x-y-z))
	 (z (third  x-y-z))
	 (r (sqrt (apply #'+ (mapcar #'square x-y-z))))
	 (φ (atan y x))
	 (θ (atan (sqrt (+ (* x x) (* y y))) z)))
    (list r φ θ)))
