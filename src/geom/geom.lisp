(defpackage #:math/geom
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
  )

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
    "@b(Описание:) функция @b(triangle-area-by-sides) возвращает
   площадь правильного треугольника со стороной @b(a).
 @b(Пример использования:)
@begin[lang=lisp](code)
 (regular-triangle-area-by-side 1.0 1.0 1.0) => 0.4330127 ; (/ (sqrt 3) 4)
 (regular-triangle-area-by-side 2.0 2.0 2.0) => 1.7320508 ; (sqrt 3)
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
  "@b(Описание:) функция @b(regular-tetrahedron-volume-by-side)
   возвращает объем правильного тетраэдра с ребром @b(a).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (regular-tetrahedron-side-by-volume 1.00) => 2.039649
 (regular-tetrahedron-side-by-volume 10.0) => 4.3942904
@end(code)"
  (expt (/ (* 12 v) (sqrt 2)) 1/3))

(defun diameter-by-radius (radius)
  (* radius 2))

(defun radius-by-diameter (diameter)
  (/ diameter 2))

(defun circle-area-by-radius (radius)
  "@b(Описание:) функция @b(circle-area-by-radius) "
  (* pi radius radius))

(defun circle-area-by-diameter (diameter)
  (circle-area-by-radius
   (radius-by-diameter diameter)))

(defun equivalent-diameter (area perimeter)
  (* 4 (/ area perimeter)))
  
