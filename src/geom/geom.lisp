;;;; ./src/geom/geom-doc.lisp

(defpackage :math/geom
  (:use #:cl)
  (:export triangle-area-by-sides
           regular-triangle-area-by-side
           regular-triangle-side-by-area)
  (:export regular-tetrahedron-volume-by-side
           regular-tetrahedron-side-by-volume)
  (:export diameter-by-radius
           radius-by-diameter)
  (:export circle-area-by-radius
           circle-area-by-diameter)
  (:export equivalent-diameter)
  (:documentation
   "@b(Описание:) Пакет @b(:math/geom) содержит функции вычисления
площадей и объёмов геометрических фигур и тел.

@begin(section) @title(Основные группы функций)

@begin(list)
 @item(@b(Треугольник:) @b(triangle-area-by-sides),
   @b(regular-triangle-area-by-side), @b(regular-triangle-side-by-area).)
 @item(@b(Тетраэдр:) @b(regular-tetrahedron-volume-by-side),
   @b(regular-tetrahedron-side-by-volume).)
 @item(@b(Окружность и круг:) @b(circle-area-by-radius), @b(circle-area-by-diameter),
   @b(diameter-by-radius), @b(radius-by-diameter).)
 @item(@b(Эквивалентный диаметр:) @b(equivalent-diameter).)
@end(list)

@end(section)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (ql:quickload :math)
 (math/geom:triangle-area-by-sides 3.0 4.0 5.0) => 6.0
 (math/geom:circle-area-by-radius 1.0)           => 3.1415927
@end(code)"))

(in-package :math/geom)

(defun triangle-area-by-sides (a b c)
    "@b(Описание:) функция @b(triangle-area-by-sides) возвращает площадь
   треугольника со сторонами @b(a), @b(b), @b(c).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (triangle-area-by-sides 1.0 1.0 1.0) => 0.4330127 ; (/ (sqrt 3) 4)
 (triangle-area-by-sides 2.0 2.0 2.0) => 1.7320508 ; (sqrt 3)
 (triangle-area-by-sides 3.0 4.0 5.0) => 6.0
 (triangle-area-by-sides 1.0 2.0 3.0) => 0.0
@end(code)"
  (let ((p (/ (+ a b c) 2)))
    (sqrt (* p (- p a) (- p b) (- p c)))))

(defun regular-triangle-area-by-side (a)
    "@b(Описание:) функция @b(regular-triangle-area-by-side) возвращает
   площадь правильного треугольника со стороной @b(a).
 @b(Пример использования:)
@begin[lang=lisp](code)
 (regular-triangle-area-by-side 1.0) => 0.4330127
 (regular-triangle-area-by-side 2.0) => 1.7320508
@end(code)"
  (triangle-area-by-sides a a a))

(defun regular-triangle-side-by-area (s)
    "@b(Описание:) функция @b(regular-triangle-side-by-area) длину стороны
   правильного треугольника с площадью @b(s).
 @b(Пример использования:)
@begin[lang=lisp](code)
 (regular-triangle-side-by-area 0.4330127) => 1.0 
 (regular-triangle-side-by-area 1.7320508) => 2.0
@end(code)"
  (sqrt (/ s (/ (sqrt 3) 4))))

(defun regular-tetrahedron-volume-by-side (a)
    "@b(Описание:) функция @b(regular-tetrahedron-volume-by-side)
   возвращает объем правильного тетраэдра с ребром @b(a).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (regular-tetrahedron-volume-by-side 1.00) => 0.11785113 
 (regular-tetrahedron-volume-by-side 10.0) => 117.85112
@end(code)"
  (* 1/12 a a a (sqrt 2)))

(defun regular-tetrahedron-side-by-volume (v)
    "@b(Описание:) функция @b(regular-tetrahedron-side-by-volume)
   возвращает длину ребра правильного тетраэдра с объёмом @b(v).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (regular-tetrahedron-side-by-volume 1.00) => 2.039649
 (regular-tetrahedron-side-by-volume 10.0) => 4.3942904
@end(code)"
  (expt (/ (* 12 v) (sqrt 2)) 1/3))

(defun diameter-by-radius (radius)
  "@b(Описание:) функция @b(diameter-by-radius) возвращает диаметр
по заданному радиусу @b(radius).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (diameter-by-radius 5.0) => 10.0
 (diameter-by-radius 1.0) => 2.0
@end(code)"
  (* radius 2))

(defun radius-by-diameter (diameter)
  "@b(Описание:) функция @b(radius-by-diameter) возвращает радиус
по заданному диаметру @b(diameter).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (radius-by-diameter 10.0) => 5.0
 (radius-by-diameter  2.0) => 1.0
@end(code)"
  (/ diameter 2))

(defun circle-area-by-radius (radius)
    "@b(Описание:) функция @b(circle-area-by-radius) возвращает площадь
круга по заданному радиусу @b(radius).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (circle-area-by-radius 1.0) => 3.141592653589793d0
 (circle-area-by-radius 2.0) => 12.566370614359172d0
@end(code)"
  (* pi radius radius))

(defun circle-area-by-diameter (diameter)
  "@b(Описание:) функция @b(circle-area-by-diameter) возвращает площадь
круга по заданному диаметру @b(diameter).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (circle-area-by-diameter 2.0) => 3.141592653589793d0
 (circle-area-by-diameter 4.0) => 12.566370614359172d0
@end(code)"
  (circle-area-by-radius
   (radius-by-diameter diameter)))

(defun equivalent-diameter (area perimeter)
  "@b(Описание:) функция @b(equivalent-diameter) возвращает эквивалентный
(гидравлический) диаметр сечения по заданным площади @b(area)
и периметру @b(perimeter).

 @b(Переменые:)
@begin(list)
 @item(area      - площадь сечения.)
 @item(perimeter - периметр сечения.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (equivalent-diameter 25.0 20.0) => 5.0 ; прямоугольник 5x5
 (equivalent-diameter  1.0  1.0) => 4.0
@end(code)

 @link[uri=\"https://ru.wikipedia.org/wiki/Гидравлический_диаметр\"](Гидравлический диаметр)
 @link[uri=\"https://en.wikipedia.org/wiki/Hydraulic_diameter\"](Hydraulic diameter)"
  (* 4 (/ area perimeter)))
  
