;;;; ./src/geom/geom-doc.lisp

(in-package :math/geom)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



(make-doc
 #'MATH/GEOM:EQUIVALENT-DIAMETER 'function
 "
 @link[uri=\"https://ru.wikipedia.org/wiki/Гидравлический_диаметр/\"](Гидравлический_диаметр)
 @link[uri=\"https://en.wikipedia.org/wiki/Hydraulic_diameter/\"](Hydraulic_diameter)
  ")

(make-doc
  #'MATH/GEOM:REGULAR-TETRAHEDRON-SIDE-BY-VOLUME 'function
  "@b(Описание:) функция @b(regular-tetrahedron-volume-by-side)
   возвращает объем правильного тетраэдра с ребром @b(a).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (regular-tetrahedron-side-by-volume 1.00) => 2.039649
 (regular-tetrahedron-side-by-volume 10.0) => 4.3942904
@end(code)")

(make-doc
  #'MATH/GEOM:REGULAR-TRIANGLE-AREA-BY-SIDE 'function
  "@b(Описание:) функция @b(triangle-area-by-sides) возвращает
   площадь правильного треугольника со стороной @b(a).
 @b(Пример использования:)
@begin[lang=lisp](code)
 (regular-triangle-area-by-side 1.0 1.0 1.0) => 0.4330127 ; (/ (sqrt 3) 4)
 (regular-triangle-area-by-side 2.0 2.0 2.0) => 1.7320508 ; (sqrt 3)
@end(code)")

(make-doc
  #'MATH/GEOM:RADIUS-BY-DIAMETER 'function
  NIL)

(make-doc
  #'MATH/GEOM:REGULAR-TRIANGLE-SIDE-BY-AREA 'function
  "@b(Описание:) функция @b(regular-triangle-side-by-area) длину стороны
   правильного треугольника с площадью @b(s).
 @b(Пример использования:)
@begin[lang=lisp](code)
 (regular-triangle-side-by-area 0.4330127) => 1.0 
 (regular-triangle-side-by-area 1.7320508) => 2.0
@end(code)")

(make-doc
  #'MATH/GEOM:CIRCLE-AREA-BY-DIAMETER 'function
  NIL)

(make-doc
  #'MATH/GEOM:DIAMETER-BY-RADIUS 'function
  NIL)

(make-doc
  #'MATH/GEOM:REGULAR-TETRAHEDRON-VOLUME-BY-SIDE 'function
  "@b(Описание:) функция @b(regular-tetrahedron-volume-by-side)
   возвращает объем правильного тетраэдра с ребром @b(a).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (regular-tetrahedron-volume-by-side 1.00) => 0.11785113 
 (regular-tetrahedron-volume-by-side 10.0) => 117.85112
@end(code)")

(make-doc
  #'MATH/GEOM:CIRCLE-AREA-BY-RADIUS 'function
  "@b(Описание:) функция @b(circle-area-by-radius) ")

(make-doc
  #'MATH/GEOM:TRIANGLE-AREA-BY-SIDES 'function
  "@b(Описание:) функция @b(triangle-area-by-sides) возвращает площадь
   треугольника со сторонами @b(a), @b(b), @b(c).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (triangle-area-by-sides 1.0 1.0 1.0) => 0.4330127 ; (/ (sqrt 3) 4)
 (triangle-area-by-sides 2.0 2.0 2.0) => 1.7320508 ; (sqrt 3)
 (triangle-area-by-sides 3.0 4.0 5.0) => 6.0
 (triangle-area-by-sides 1.0 2.0 3.0) => 0.0
@end(code)")
