;;;; coordinate-system.lisp


(defpackage #:math/coord
  (:use #:cl)
  (:export dtr
	   rtd
	   polar->cartesian
	   cartesian->polar
	   spherical->cartesian
	   cartesian->spherical
	   ))

(in-package :math/coord)

(export 'dtr )
(defun dtr (degree)
"@b(Описание:) функция @b(dtr) переводит значение, заданное в градусах, в радианы.

 @b(Пример использования:)

@begin[lang=lisp](code)
 (dtr (rtd 1/2)) => 0.5d0
@end(code)
"
  (* pi 1/180 degree ))

(export 'rtd )
(defun rtd (radian)
"@b(Описание:) функция @b(rtd) переводит значение, 
заданное в радианах, в градусы.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (rtd (dtr 45)) => 45.0d0
@end(code)
"
  (/ radian pi 1/180))

(export 'polar->cartesian )
(defun polar->cartesian (radius-angle)
"@b(Описание:) функция @b(polar->cartesian) переводит полярные координаты в декартовы.

 @b(Переменые:)
@begin(list)
 @item(radius-angle - список, состоящий из двух элементов: радиус-вектора 
и угла, заданного в радианах.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (polar->cartesian (list 10.0 (dtr 45))) => (7.0710678118654755d0 7.071067811865475d0)
@end(code)
"
  (let ((radius (first  radius-angle))
	(angle  (second radius-angle)))
    (list (* radius (cos angle)) (* radius (sin angle)))))

(export 'cartesian->polar )
(defun cartesian->polar (x-y)
"@b(Описание:) функция @b(cartesian->polar) переводит декартовы координаты в полярные.

 @b(Переменые:)
@begin(list)
 @item(radius-angle - список, состоящий из двух элементов: радиус-вектора 
и угла, заданного в радианах.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (cartesian->polar (list 10.0 (dtr 45))) => (10.030795096853893d0 0.07837892038914972d0) 
@end(code)
"
  (let ((radius (sqrt (apply #'+ (mapcar #'square x-y))))
	(angle  (atan (second x-y) (first x-y))))
    (list radius angle)))

(export 'spherical->cartesian )
(defun spherical->cartesian (r φ θ)
"@b(Описание:) функция @b(spherical->cartesian) выполняет преобразование координат 
из сферических в декартовы.

 @b(Переменые:)
@begin(list)
 @item(r - расстояние от начала координат до заданной точки;)
 @item(φ - азимутальный угол (в плоскости X0Y);)
 @item(θ - зенитный угол.)
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (spherical->cartesian 100 (dtr 30) (dtr 45)) 
 => (61.237243569579455d0 35.35533905932737d0 70.71067811865476d0)
@end(code)
"
  (let ((x (* r (cos φ) (sin θ)))
	(y (* r (sin φ) (sin θ)))
	(z (* r (cos θ))))
    (list x y z)))

(export 'cartesian->spherical )
(defun cartesian->spherical (x-y-z)
"@b(Описание:) функция @b(cartesian->spherical) выполняет преобразование координат 
из декартовых в сферические.

 @b(Переменые:)
@begin(list)
 @item(x-y-z - список, содержащий соответствующие координаты.)
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (cartesian->spherical '(61.237243569579455d0 35.35533905932737d0 70.71067811865476d0))
 =>(100.0d0 0.5235987755982988d0 0.7853981633974483d0)
@end(code)
"
  (let* ((x (first  x-y-z))
	 (y (second x-y-z))
	 (z (third  x-y-z))
	 (r (sqrt (apply #'+ (mapcar #'square x-y-z))))
	 (φ (atan y x))
	 (θ (atan (sqrt (+ (* x x) (* y y))) z)))
    (list r φ θ)))
