;;;; coordinate-system.lisp

(in-package #:math)

(export 'dtr)
(defun dtr (degree) (* pi 1/180 degree ))

(export 'rtd)
(defun rtd (radian) (/ radian pi 1/180))
    
;;;; (rtd (dtr 45))

(export 'polar->cartesian)
(defun polar->cartesian (radius-angle)
  (let ((radius (first  radius-angle))
	(angle  (second radius-angle)))
    (list (* radius (cos angle)) (* radius (sin angle)))))

(export 'cartesian->polar)
(defun cartesian->polar (x-y)
  (let ((radius (sqrt (apply #'+ (mapcar #'square x-y))))
	(angle  (atan (second x-y) (first x-y))))
    (list radius angle)))

(export 'spherical->cartesian)
(defun spherical->cartesian (r φ θ)
  "Выполняет преобразование координат из сферических в полярые
 r - расстояние от начала координат до заданной точки;
 φ - азимутальный угол (в плоскости X0Y);
 θ - зенитный угол;
"
  (let ((x (* r (cos φ) (sin θ)))
	(y (* r (sin φ) (sin θ)))
	(z (* r (cos θ))))
    (list x y z)))

;;;; (cartesian->spherical (spherical->cartesian 100 (dtr 30) (dtr 45)))

(export 'cartesian->spherical)
(defun cartesian->spherical (x-y-z)
  (let* ((x (first  x-y-z))
	 (y (second x-y-z))
	 (z (third  x-y-z))
	 (r (sqrt (apply #'+ (mapcar #'square x-y-z))))
	 (φ (atan y x))
	 (θ (atan (sqrt (+ (* x x) (* y y))) z)))
    (list r φ θ)))

;;;; (cartesian->polar (polar->cartesian (list 5 (dtr 45))))


;;;; (cartesian->polar '(5d0 3d0))
