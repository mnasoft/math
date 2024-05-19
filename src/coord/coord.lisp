;;;; ./src/coord/coordinate-system.lisp
 
(defpackage :math/coord
  (:use #:cl #:math/core)
  (:export dtr
           rtd
           polar->cartesian
           cartesian->polar
           spherical->cartesian
           cartesian->spherical
           )
  (:export point-3d->4d
           point-4d->3d
           )
  (:export double-float-list
           single-float-list
           )
  (:documentation "coord"))

(in-package :math/coord)

(defun dtr (degree)
  "@b(Описание:) функция @b(dtr) переводит значение, заданное в градусах, в радианы.

 @b(Пример использования:)

@begin[lang=lisp](code)
 (dtr (rtd 1/2)) => 0.5d0
@end(code)
"  
  (* pi 1/180 degree ))

(defun rtd (radian)
  "@b(Описание:) функция @b(rtd) переводит значение, 
заданное в радианах, в градусы.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (rtd (dtr 45)) => 45.0d0
@end(code)
"  
  (/ radian pi 1/180))

(defun polar->cartesian (radius-angle)
  "@b(Описание:) функция @b(polar->cartesian) переводит полярные координаты в декартовы.

 @b(Переменые:)
@begin(list)
 @item(radius-angle - список, состоящий из двух элементов: радиус-вектора 
и угла, заданного в радианах.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (polar->cartesian `(10.0 ,(dtr 45))) => (7.0710678118654755d0 7.071067811865475d0)
@end(code)
"  
  (let ((radius (first  radius-angle))
	(angle  (second radius-angle)))
    (list (* radius (cos angle)) (* radius (sin angle)))))

(defun cartesian->polar (x-y)
  "@b(Описание:) функция @b(cartesian->polar) переводит декартовы координаты в полярные.

 @b(Переменые:)
@begin(list)
 @item(radius-angle - список, состоящий из двух элементов: радиус-вектора 
и угла, заданного в радианах.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (cartesian->polar (list 10.0 10)) (14.142136 0.7853982)
@end(code)
"  
  (let ((radius (sqrt (apply #'+ (mapcar #'square x-y))))
	(angle  (atan (second x-y) (first x-y))))
    (list radius angle)))

(defun spherical->cartesian (r-φ-θ)
  "@b(Описание:) функция @b(spherical->cartesian) выполняет
преобразование координат из сферических в декартовы.

 @b(Переменые:)
@begin(list)
 @item(r-φ-θ - список состоящий из трех величин:
  @begin(list)
   @item(r - расстояние от начала координат до заданной точки;)
   @item(φ - азимутальный угол (в плоскости X0Y);)
   @item(θ - зенитный угол.)
@end(list))
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (spherical->cartesian `(100 ,(dtr 30) ,(dtr 45)))
 => (61.237243569579455d0 35.35533905932737d0 70.71067811865476d0)
@end(code)
"  
  (let ((r (first  r-φ-θ))
        (φ (second r-φ-θ))
        (θ (third  r-φ-θ)))
    (let ((x (* r (cos φ) (sin θ)))
	  (y (* r (sin φ) (sin θ)))
	  (z (* r (cos θ))))
      (list x y z))))

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

(defun point-3d->4d (point-3d &optional (coord-4 1.0d0))
  "@b(Описание:) функция @b(point-3d->4d) возвращает координаты точки
 @b(point-3d) в однородных координатах, добавляя к ним четвертую
 координату @b(coord-4).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (point-3d->4d '(1.0 2.0 3.0))     => (1.0 2.0 3.0 0.0d0)
 (point-3d->4d '(1.0 2.0 3.0) 0.0) => (1.0 2.0 3.0 0.0)
@end(code)"
  (let ((point-4d (nreverse(copy-list point-3d))))
    (nreverse (push coord-4 point-4d))))

(defun point-4d->3d (point-4d)
  "@b(Описание:) функция @b(point-4d->3d) преобразует координаты точки
 @b(point-4d) из однородных в 3d и возвращает последние.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (point-4d->3d '(1.0 2.0 3.0 0.0d0)) => (1.0 2.0 3.0)
@end(code)"
  (loop :for i :from 0 :to 2
        :for coord :in point-4d
        :collect coord))

(defun double-float-list (list)
  (loop :for i :in list
        :collect (coerce i 'double-float)))

(defun single-float-list (list)
  (loop :for i :in list
        :collect (coerce i 'single-float)))
