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
  (:documentation
   "@b(Описание:) Пакет @b(:math/coord) содержит функции преобразования
угловой меры и координат точки между системами координат.

@begin(section) @title(Основные группы функций)

@begin(list)
 @item(@b(Угловая мера:) @b(dtr) — градусы в радианы, @b(rtd) — радианы в градусы.)
 @item(@b(Полярные координаты:) @b(polar->cartesian), @b(cartesian->polar).)
 @item(@b(Сферические координаты:) @b(spherical->cartesian), @b(cartesian->spherical).)
 @item(@b(Преобразование точек:) @b(point-3d->4d), @b(point-4d->3d).)
 @item(@b(Преобразование типов:) @b(double-float-list), @b(single-float-list).)
@end(list)

@end(section)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (ql:quickload :math)
 (math/coord:dtr 180.0)                    => 3.1415927
 (math/coord:polar->cartesian 1.0 0.0)    => (1.0 0.0)
@end(code)"))

(in-package :math/coord)

(defun dtr (degree)
  "@b(Описание:) функция @b(dtr) переводит значение, заданное в градусах, в радианы.

 @b(Переменые:)
@begin(list)
 @item(degree - угол в градусах.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dtr 180) => 3.141592653589793d0
 (dtr  90) => 1.5707963267948966d0
 (dtr  45) => 0.7853981633974483d0
@end(code)
"  
  (* pi 1/180 degree ))

(defun rtd (radian)
  "@b(Описание:) функция @b(rtd) переводит значение,
заданное в радианах, в градусы.

 @b(Переменые:)
@begin(list)
 @item(radian - угол в радианах.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (rtd pi)       => 180.0d0
 (rtd (/ pi 2)) => 90.0d0
 (rtd (/ pi 4)) => 45.0d0
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
 (polar->cartesian (list 10.0 (dtr 45))) => (7.0710678118654755d0 7.071067811865475d0)
 (polar->cartesian (list  1.0 0.0))      => (1.0 0.0)
@end(code)
"  
  (let ((radius (first  radius-angle))
	(angle  (second radius-angle)))
    (list (* radius (cos angle)) (* radius (sin angle)))))

(defun cartesian->polar (x-y)
  "@b(Описание:) функция @b(cartesian->polar) переводит декартовы координаты в полярные.

 @b(Переменые:)
@begin(list)
 @item(x-y - список, состоящий из двух элементов: координат точки x и y.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (cartesian->polar (list 10.0 10.0)) => (14.142136 0.7853982)
 (cartesian->polar (list  1.0  0.0)) => (1.0 0.0)
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
  "@b(Описание:) функция @b(double-float-list) преобразует элементы
списка @b(list) в значения типа @b(double-float).

 @b(Переменые:)
@begin(list)
 @item(list - список числовых значений.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (double-float-list '(1 2 3))   => (1.0d0 2.0d0 3.0d0)
 (double-float-list '(1/2 3/4)) => (0.5d0 0.75d0)
@end(code)"
  (loop :for i :in list
        :collect (coerce i 'double-float)))

(defun single-float-list (list)
  "@b(Описание:) функция @b(single-float-list) преобразует элементы
списка @b(list) в значения типа @b(single-float).

 @b(Переменые:)
@begin(list)
 @item(list - список числовых значений.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (single-float-list '(1 2 3))   => (1.0 2.0 3.0)
 (single-float-list '(1/2 3/4)) => (0.5 0.75)
@end(code)"
  (loop :for i :in list
        :collect (coerce i 'single-float)))
